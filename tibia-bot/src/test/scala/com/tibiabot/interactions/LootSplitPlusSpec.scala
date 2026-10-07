package com.tibiabot.interactions

import net.dv8tion.jda.api.components.label.Label
import net.dv8tion.jda.api.modals.Modal
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

/** The expenses form is one box per player, and Discord rejects a sixth box.
 *  A party of six has to come back as two forms of five and one, not one form
 *  of six that fails when somebody opens it.
 */
class LootSplitPlusSpec extends AnyFunSuite with Matchers {

  private val party = List("Decay Skipuje Tury", "Hansmann", "Kubdrut", "Oli Giga Spell", "Simek Giga Maczuga", "Avalanche Blond Loczek")

  private def labels(modal: Modal): List[String] =
    modal.getComponents.asScala.toList.map(_.asInstanceOf[Label].getLabel)

  test("one page lists every player when the party fits") {
    val modal = LootSplitPlus.expensesModal(party.take(5), "token", 0)
    modal.getTitle shouldBe "Extra expenses"
    labels(modal) shouldBe party.take(5)
    modal.getComponents.size should be <= Modal.MAX_COMPONENTS
  }

  test("a sixth player is the next page, not a sixth box") {
    val first = LootSplitPlus.expensesModal(party, "token", 0)
    val second = LootSplitPlus.expensesModal(party, "token", 1)
    first.getComponents should have size Modal.MAX_COMPONENTS
    labels(first) shouldBe party.take(5)
    labels(second) shouldBe List("Avalanche Blond Loczek")
    second.getTitle shouldBe "Extra expenses 6"
  }
}
