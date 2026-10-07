package com.tibiabot.interactions

import com.tibiabot.lootsplit.{ExtraSupplies, HuntSession, LootSplitIds}
import com.tibiabot.presentation.{Embeds, LootSplitEmbeds}
import com.tibiabot.Config
import net.dv8tion.jda.api.components.actionrow.ActionRow
import net.dv8tion.jda.api.components.buttons.Button
import net.dv8tion.jda.api.components.label.Label
import net.dv8tion.jda.api.components.textinput.{TextInput, TextInputStyle}
import net.dv8tion.jda.api.events.interaction.ModalInteractionEvent
import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent
import net.dv8tion.jda.api.modals.Modal

import java.util.UUID
import scala.collection.concurrent.TrieMap

/** `/lootsplitplus` — one hunt, then a form that adds supplies the analyser
 *  never saw.
 *
 *  Two forms rather than one, because the second one is built from the names in
 *  the first, and Discord will not open a form from a form. The paste is
 *  answered with a button; the button opens "Extra expenses", one box per
 *  player. A party bigger than Discord's five boxes is the same form again for
 *  the next five, and the split is sent only when the last page is in.
 *
 *  The paste is remembered under a token on the button, not in the button: a
 *  component id holds a hundred characters and a hunt does not. It lives for
 *  half an hour and only for the person who pasted it.
 */
object LootSplitPlus {

  private val PageSize: Int = Modal.MAX_COMPONENTS
  private val TtlMillis: Long = 30L * 60L * 1000L
  private val PasteField: String = "analyser"

  private final case class Pending(userId: String, pasted: String, hunt: HuntSession, amounts: Vector[Long], created: Long)

  private val pending = TrieMap.empty[String, Pending]

  def handlesButton(componentId: String): Boolean = LootSplitIds.handlesPlusButton(componentId)

  def handlesModal(modalId: String): Boolean = LootSplitIds.handlesPlusModal(modalId)

  def pasteModal: Modal =
    Modal.create(LootSplitIds.PlusModal, "Loot split — one hunt")
      .addComponents(Label.of(
        "Hunt",
        "One session, copied from the party window. Extra expenses come next.",
        TextInput.create(PasteField, TextInputStyle.PARAGRAPH)
          .setRequired(true)
          .setMaxLength(TextInput.MAX_VALUE_LENGTH)
          .setPlaceholder("Session data: From 2026-09-01, 21:12:00 to 2026-09-01, 23:29:40")
          .build()
      ))
      .build()

  /** One short box per player on this page. The custom id is the index on the
   *  page, not the name: a name can be long, and the index is what lines the
   *  amount back up with the member list. */
  def expensesModal(names: List[String], token: String, page: Int): Modal = {
    val from = page * PageSize
    val slice = names.slice(from, from + PageSize)
    val last = from + slice.size
    val title =
      if (names.size <= PageSize) "Extra expenses"
      else if (slice.size == 1) s"Extra expenses $last"
      else s"Extra expenses ${from + 1}–$last"
    Modal.create(LootSplitIds.plusExpenses(token, page), title.take(45))
      .addComponents(slice.zipWithIndex.map { case (name, index) =>
        Label.of(
          label(name),
          "Added to their supplies. Blank means nothing.",
          TextInput.create(s"e$index", TextInputStyle.SHORT)
            .setRequired(false)
            .setMaxLength(20)
            .setPlaceholder("0")
            .build()
        )
      }: _*)
      .build()
  }

  def handleModal(event: ModalInteractionEvent): Unit =
    if (event.getModalId == LootSplitIds.PlusModal) handlePaste(event)
    else handleExpenses(event)

  def handleButton(event: ButtonInteractionEvent): Unit =
    parts(event.getComponentId, LootSplitIds.PlusOpen) match {
      case None =>
        event.replyEmbeds(Embeds.response(s"${Config.noEmoji} That button doesn't open a hunt anymore.")).setEphemeral(true).queue()
      case Some((token, page)) =>
        stored(token, event.getUser.getId) match {
          case None =>
            event.replyEmbeds(Embeds.response(s"${Config.noEmoji} That hunt expired. Run /lootsplitplus again.")).setEphemeral(true).queue()
          case Some(item) =>
            event.replyModal(expensesModal(item.hunt.members.map(_.name), token, page)).queue()
        }
    }

  private def handlePaste(event: ModalInteractionEvent): Unit = {
    val pasted = Option(event.getValue(PasteField)).map(_.getAsString).getOrElse("")
    ExtraSupplies.singleHunt(pasted) match {
      case Left(problem) =>
        event.replyEmbeds(Embeds.response(s"${Config.noEmoji} $problem")).setEphemeral(true).queue()
      case Right(hunt) if hunt.members.isEmpty =>
        publish(event, hunt, pasted)
      case Right(hunt) =>
        sweep()
        val token = UUID.randomUUID().toString.replace("-", "")
        pending(token) = Pending(event.getUser.getId, pasted, hunt, Vector.fill(hunt.members.size)(0L), System.currentTimeMillis())
        val prompt = event.replyEmbeds(Embeds.response(
          s"${hunt.members.size} players in this hunt. Add extra supplies for anyone who paid on top — it comes off the balance."
        )).setEphemeral(true).setComponents(ActionRow.of(Button.primary(LootSplitIds.plusOpen(token, 0), openLabel(hunt.members.size, 0))))
        prompt.queue()
    }
  }

  private def handleExpenses(event: ModalInteractionEvent): Unit =
    parts(event.getModalId, LootSplitIds.PlusExpenses) match {
      case None =>
        event.replyEmbeds(Embeds.response(s"${Config.noEmoji} That form doesn't belong to a hunt anymore.")).setEphemeral(true).queue()
      case Some((token, page)) =>
        stored(token, event.getUser.getId) match {
          case None =>
            event.replyEmbeds(Embeds.response(s"${Config.noEmoji} That hunt expired. Run /lootsplitplus again.")).setEphemeral(true).queue()
          case Some(item) =>
            val from = page * PageSize
            val slice = item.hunt.members.slice(from, from + PageSize)
            val raws = slice.indices.toList.map(index => Option(event.getValue(s"e$index")).map(_.getAsString).getOrElse(""))
            ExtraSupplies.readPage(slice.map(_.name), raws) match {
              case Left(problem) =>
                event.replyEmbeds(Embeds.response(s"${Config.noEmoji} $problem")).setEphemeral(true).queue()
              case Right(pageAmounts) =>
                val amounts = item.amounts.patch(from, pageAmounts, pageAmounts.size)
                val next = from + slice.size
                if (next < item.hunt.members.size) {
                  pending(token) = item.copy(amounts = amounts)
                  event.replyEmbeds(Embeds.response(s"Saved. ${item.hunt.members.size - next} players still to go."))
                    .setEphemeral(true)
                    .setComponents(ActionRow.of(Button.primary(
                      LootSplitIds.plusOpen(token, page + 1),
                      openLabel(item.hunt.members.size, page + 1)
                    )))
                    .queue()
                } else {
                  pending.remove(token)
                  publish(event, item.hunt.withExtraSupplies(amounts), item.pasted)
                }
            }
        }
    }

  private def publish(event: ModalInteractionEvent, hunt: HuntSession, pasted: String): Unit =
    event.replyEmbeds(LootSplitEmbeds.session(hunt, Config.goldEmoji))
      .addFiles(LootSplitEmbeds.paste(pasted))
      .queue()

  /** The person who pasted, and only while the token is still fresh. Anyone
   *  else — or the same person half an hour later — gets nothing, which is also
   *  why a token is not a secret worth keeping. */
  private def stored(token: String, userId: String): Option[Pending] = {
    sweep()
    pending.get(token).filter(item => item.userId == userId && System.currentTimeMillis() - item.created < TtlMillis)
  }

  private def sweep(): Unit = {
    val now = System.currentTimeMillis()
    pending.filterInPlace { case (_, item) => now - item.created < TtlMillis }
  }

  private def parts(id: String, prefix: String): Option[(String, Int)] = {
    val rest = id.stripPrefix(prefix)
    rest.split(":", 2) match {
      case Array(token, page) if token.nonEmpty => page.toIntOption.filter(_ >= 0).map(token -> _)
      case _                                    => None
    }
  }

  private def openLabel(memberCount: Int, page: Int): String = {
    val from = page * PageSize
    val until = math.min(from + PageSize, memberCount)
    if (memberCount <= PageSize) "Extra expenses"
    else s"Players ${from + 1}–$until"
  }

  /** A label Discord will accept. A Tibia name fits; anything longer is cut
   *  rather than failing the whole form. */
  private def label(name: String): String =
    if (name.length <= 45) name else name.take(44).trim + "…"
}
