package com.tibiabot.interactions

import com.tibiabot.lootsplit.LootSplitIds
import net.dv8tion.jda.api.modals.Modal
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Discord rejects a sixth box, and the parse ceiling is ten hunts rather than
 *  five fields — so the form has to sit on both limits at once. */
class LootSplitSpec extends AnyFunSuite with Matchers {

  test("the form uses every slot Discord allows, and no more") {
    LootSplit.modal.getComponents.size shouldBe Modal.MAX_COMPONENTS
    LootSplitIds.PasteFields should have size Modal.MAX_COMPONENTS
  }
}
