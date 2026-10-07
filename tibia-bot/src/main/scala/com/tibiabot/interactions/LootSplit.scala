package com.tibiabot.interactions

import com.tibiabot.lootsplit.{HuntAnalyser, LootSplitIds}
import com.tibiabot.presentation.{Embeds, LootSplitEmbeds}
import com.tibiabot.Config
import com.typesafe.scalalogging.StrictLogging
import net.dv8tion.jda.api.components.label.Label
import net.dv8tion.jda.api.components.textinput.{TextInput, TextInputStyle}
import net.dv8tion.jda.api.events.interaction.ModalInteractionEvent
import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent
import net.dv8tion.jda.api.modals.Modal

/** The Loot Split form: a box to paste a party hunt analyser into, and the split
 *  that comes back.
 *
 *  ==Why neither half is deferred==
 *  Every other interaction in this bot is acknowledged up front because it has
 *  database or REST work to do before it can say anything. This one has none: the
 *  whole input arrives inside the interaction, the parse is string work on a few
 *  pasted hunts, and the embed is built from the result. So both halves
 *  answer on JDA's event thread rather than queueing for a worker — which is not
 *  just cheap but necessary, since a press that opens a modal cannot be deferred at
 *  all and a submission that cannot be deferred ephemerally-or-not depending on the
 *  outcome has to choose after the parse has run.
 *
 *  That last point is the reason this does not live in [[RespawnModals]] despite the
 *  only button being on a respawn DM: a split has to arrive as an ordinary message,
 *  since the whole use of it is copying three lines out an hour later, while a paste
 *  that failed to read should be ephemeral so a guild channel is not left holding
 *  somebody's typo. That same hour is why a split carries the analyser text back
 *  as a file — see [[com.tibiabot.presentation.LootSplitEmbeds.paste]]. `RespawnModals` also refuses anything
 *  submitted with no guild, which is every submission from a DM.
 */
object LootSplit extends StrictLogging {

  def handlesButton(componentId: String): Boolean = LootSplitIds.handlesButton(componentId)

  def handlesModal(modalId: String): Boolean = LootSplitIds.handlesModal(modalId)

  /** Five paste boxes — Discord will not take a sixth — each at the paragraph
   *  ceiling. A four-member session is about six hundred characters, so one box
   *  holds several hunts pasted back to back; ten hunts in total is the parse
   *  ceiling. A party of ~20 still overruns a single box, which the parser
   *  reports as a cut-off paste rather than splitting what survived. */
  def modal: Modal =
    Modal.create(LootSplitIds.Modal, "Loot split — up to 10 hunts")
      .addComponents(pasteBoxes: _*)
      .build()

  private def pasteBoxes: Seq[Label] = {
    val placeholder = "Session data: From 2026-09-01, 21:12:00 to 2026-09-01, 23:29:40"
    LootSplitIds.PasteFields.zipWithIndex.map { case (id, index) =>
      val (title, description, required) =
        if (index == 0)
          ("Hunt 1", "Copy from the party hunt window. Several hunts can go in one box.", true)
        else if (index == 4)
          ("Hunts 5–10", "Optional. More sessions, one after another if you have them.", false)
        else
          (s"Hunt ${index + 1}", "Optional. Another session, or several pasted together.", false)
      Label.of(
        title,
        description,
        TextInput.create(id, TextInputStyle.PARAGRAPH)
          .setRequired(required)
          .setMaxLength(TextInput.MAX_VALUE_LENGTH)
          .setPlaceholder(placeholder)
          .build()
      )
    }
  }

  def handleButton(event: ButtonInteractionEvent): Unit = event.replyModal(modal).queue()

  def handleModal(event: ModalInteractionEvent): Unit = {
    val pasted = LootSplitIds.PasteFields
      .flatMap(id => Option(event.getValue(id)).map(_.getAsString))
      .map(_.trim)
      .filter(_.nonEmpty)
      .mkString("\n")
    HuntAnalyser.parse(pasted) match {
      case Left(problem) =>
        // Ephemeral, and the button left live: whoever pasted still needs a split,
        // and the fix is to paste again.
        event.replyEmbeds(Embeds.response(s"${Config.noEmoji} $problem")).setEphemeral(true).queue()
      case Right(hunt) =>
        // The paste goes up with the split rather than instead of it: the modal
        // that carried it is gone as soon as this returns, and the embed is a
        // reading rather than a copy.
        event.replyEmbeds(LootSplitEmbeds.session(hunt, Config.goldEmoji))
          .addFiles(LootSplitEmbeds.paste(pasted))
          .queue()
        spendButton(event)
    }
  }

  /** Grey out the button this form was opened from, now that it has produced the
   *  split it exists to produce.
   *
   *  Null when the form came from `/lootsplit`, which has no message behind it —
   *  so the same handler covers both ways in without asking which it was. Failures
   *  are swallowed to a log line: the split has already been sent by this point,
   *  and a message the bot can no longer edit is not worth a second reply about.
   */
  private def spendButton(event: ModalInteractionEvent): Unit =
    Option(event.getMessage).foreach { message =>
      message.editMessageComponents(LootSplitIds.spentRow).queue(
        _ => (),
        error => logger.debug(s"Could not retire the Loot Split button on message '${message.getId}': ${error.getMessage}")
      )
    }
}
