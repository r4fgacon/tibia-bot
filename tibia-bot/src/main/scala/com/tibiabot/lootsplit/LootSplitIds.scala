package com.tibiabot.lootsplit

import net.dv8tion.jda.api.components.actionrow.ActionRow
import net.dv8tion.jda.api.components.buttons.Button

/** The component ids behind the loot split, and the one row that carries them.
 *
 *  Its own namespace rather than a `respawn:` id, even though the only button
 *  drawn today sits on a respawn DM: nothing here touches a claim, a spawn or the
 *  database, and `RespawnModals` refuses outright anything submitted where there
 *  is no guild — which a DM is. Kept out of `interactions` so the respawn service
 *  can draw the row without the respawn package depending on the handlers that
 *  answer it.
 *
 *  Neither id carries any state. The whole input is the pasted text, so there is
 *  nothing to carry.
 */
object LootSplitIds {
  val Prefix: String = "lootsplit:"

  /** Opens the paste form. On the claim-ended DM today. */
  val Open: String = s"${Prefix}open"

  val Modal: String = s"${Prefix}modal"

  /** `/lootsplitplus` pastes one hunt here, then a button opens the expenses form.
   *  The button and that form carry a token: the paste itself does not fit in a
   *  component id, so the token is a key into a short-lived map. */
  val PlusModal: String = s"${Prefix}plus"
  val PlusOpen: String = s"${Prefix}px:"
  val PlusExpenses: String = s"${Prefix}pm:"

  def plusOpen(token: String, page: Int): String = s"$PlusOpen$token:$page"

  def plusExpenses(token: String, page: Int): String = s"$PlusExpenses$token:$page"

  /** Five paste boxes — Discord's modal ceiling — so an evening of hunts can be
   *  split without concatenating them by hand. Each box still accepts several
   *  sessions pasted one after another; ten hunts across the lot is the parse
   *  ceiling, not five. */
  val PasteFields: List[String] = (1 to 5).map(n => s"analyser$n").toList

  val Label: String = "Loot Split"

  def handlesButton(componentId: String): Boolean = componentId == Open

  def handlesModal(modalId: String): Boolean = modalId == Modal

  def handlesPlusButton(componentId: String): Boolean = componentId.startsWith(PlusOpen)

  def handlesPlusModal(modalId: String): Boolean = modalId == PlusModal || modalId.startsWith(PlusExpenses)

  def button: Button = Button.primary(Open, Label)

  def buttonRow: ActionRow = ActionRow.of(button)

  /** What the row is replaced with once a split has actually been produced from it.
   *
   *  Greyed out rather than taken away: the DM keeps its shape, and a button that
   *  is visibly spent explains the split sitting underneath it better than an empty
   *  space would. Only a successful split spends it — a paste that failed to read
   *  leaves it live, since the person still has to get one. */
  def spentRow: ActionRow = ActionRow.of(button.asDisabled)
}
