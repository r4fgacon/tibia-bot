package com.tibiabot.lootsplit

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Extra supplies are gold the analyser never saw. They have to move the same
 *  numbers the split is built from, or the transfers would still square the
 *  party to the client's balance and quietly ignore what was just typed.
 */
class ExtraSuppliesSpec extends AnyFunSuite with Matchers {

  private def member(name: String, balance: Long) =
    HuntMember(name, loot = balance, supplies = 0, balance = balance, damage = 1, healing = 1, leader = false)

  private val hunt = HuntSession(
    from = None, to = None, sessionLabel = "01:00h", lootType = "Leader",
    loot = 300L, supplies = 0L, balance = 300L,
    members = List(member("Decay Skipuje Tury", 200L), member("Hansmann", 100L))
  )

  test("a blank box is zero and a comma-grouped amount is gold") {
    ExtraSupplies.read("Hansmann", "  ") shouldBe Right(0L)
    ExtraSupplies.read("Hansmann", "50,000") shouldBe Right(50000L)
  }

  test("a minus is refused rather than taken as a refund") {
    ExtraSupplies.read("Hansmann", "-10").left.getOrElse("") should include("Hansmann")
  }

  test("one bad box fails the page and names who it was") {
    val problem = ExtraSupplies.readPage(List("Decay Skipuje Tury", "Hansmann"), List("1000", "lots")).left.getOrElse("")
    problem should include("Hansmann")
  }

  test("extra supplies raise that player's supplies and lower both balances") {
    val adjusted = hunt.withExtraSupplies(List(50L, 0L))
    adjusted.loot shouldBe 300L
    adjusted.supplies shouldBe 50L
    adjusted.balance shouldBe 250L
    adjusted.members.head.supplies shouldBe 50L
    adjusted.members.head.balance shouldBe 150L
    adjusted.members(1).balance shouldBe 100L
    adjusted.extraSupplies shouldBe List("Decay Skipuje Tury" -> 50L)
    adjusted.individualBalance shouldBe 125L
  }

  test("nothing typed leaves the hunt as the analyser had it") {
    hunt.withExtraSupplies(List(0L, 0L)) shouldBe hunt
  }

  test("several hunts are refused, because this form lists one party") {
    val one = List(
      "Session data: From 2026-10-06, 21:00:00 to 2026-10-06, 22:00:00",
      "Session: 01:00h",
      "Loot Type: Leader",
      "Loot: 100",
      "Supplies: 0",
      "Balance: 100",
      "Alpha",
      "Loot: 100",
      "Supplies: 0",
      "Balance: 100"
    ).mkString("\n")
    ExtraSupplies.singleHunt(one + "\n" + one).left.getOrElse("") should include("2 hunts")
    ExtraSupplies.singleHunt(one).map(_.members.map(_.name)) shouldBe Right(List("Alpha"))
  }
}
