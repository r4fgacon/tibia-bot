package com.tibiabot.lootsplit

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** Pins the analyser reader and the split it feeds against a real session copied
 *  out of the client, numbers and all. Every expected figure here was checked
 *  against what the game itself reported for the same hunt.
 */
class HuntAnalyserSpec extends AnyFunSuite with Matchers with OptionValues {

  /** A real four-member session, tabs and all — this is byte-for-byte what the
   *  client puts on the clipboard. */
  private val Paste: String =
    List(
      "Session data: From 2026-09-01, 21:12:00 to 2026-09-01, 23:29:40",
      "Session: 02:17h",
      "Loot Type: Leader",
      "Loot: 14,359,954",
      "Supplies: 5,354,392",
      "Balance: 9,005,562",
      "Boss Jeremy",
      "\tLoot: 1,654,034",
      "\tSupplies: 1,591,724",
      "\tBalance: 62,310",
      "\tDamage: 14,380,542",
      "\tHealing: 3,755,746",
      "Neutrul The Wise",
      "\tLoot: 50,000",
      "\tSupplies: 798,351",
      "\tBalance: -748,351",
      "\tDamage: 17,679,880",
      "\tHealing: 11,307,154",
      "The Wingga (Leader)",
      "\tLoot: 11,518,496",
      "\tSupplies: 1,718,946",
      "\tBalance: 9,799,550",
      "\tDamage: 10,774,182",
      "\tHealing: 5,877,463",
      "Violent Beams",
      "\tLoot: 1,137,424",
      "\tSupplies: 1,245,371",
      "\tBalance: -107,947",
      "\tDamage: 25,228,828",
      "\tHealing: 3,970,719"
    ).mkString("\n")

  /** The sentence a bad paste comes back with, or a failed test if it parsed. */
  private def refusal(text: String): String =
    HuntAnalyser.parse(text).swap.getOrElse(fail("expected a refusal, got a session"))

  private def parsed(text: String = Paste): HuntSession =
    HuntAnalyser.parse(text) match {
      case Right(session) => session
      case Left(problem)  => fail(s"expected a session, got: $problem")
    }

  // --- reading ------------------------------------------------------------

  test("reads the header block") {
    val hunt = parsed()
    hunt.from shouldBe Some(LocalDateTime.of(2026, 9, 1, 21, 12, 0))
    hunt.to shouldBe Some(LocalDateTime.of(2026, 9, 1, 23, 29, 40))
    hunt.sessionLabel shouldBe "02:17h"
    hunt.lootType shouldBe "Leader"
    hunt.loot shouldBe 14359954L
    hunt.supplies shouldBe 5354392L
    hunt.balance shouldBe 9005562L
  }

  test("reads every member, in the order the client listed them") {
    parsed().members.map(_.name) shouldBe
      List("Boss Jeremy", "Neutrul The Wise", "The Wingga", "Violent Beams")
  }

  test("a negative balance keeps its sign") {
    parsed().members.find(_.name == "Neutrul The Wise").map(_.balance) shouldBe Some(-748351L)
  }

  test("the (Leader) marker is read and then dropped from the name") {
    val leader = parsed().members.find(_.leader)
    leader.map(_.name) shouldBe Some("The Wingga")
    parsed().members.filter(_.leader) should have size 1
  }

  test("a member's own figures are read from their own block, not the header's") {
    val wingga = parsed().members.find(_.name == "The Wingga").value
    wingga.loot shouldBe 11518496L
    wingga.supplies shouldBe 1718946L
    wingga.balance shouldBe 9799550L
    wingga.damage shouldBe 10774182L
    wingga.healing shouldBe 5877463L
  }

  /** The reason the reader keys off `<known key>:` rather than the leading tab:
   *  the text travels through a clipboard and a Discord textarea to get here, and
   *  indentation is the first thing that route loses. */
  test("a paste whose indentation was eaten reads exactly the same") {
    parsed(Paste.replace("\t", "")) shouldBe parsed()
  }

  test("blank lines and trailing whitespace are ignored") {
    parsed(Paste.replace("\n", "  \n\n")) shouldBe parsed()
  }

  test("a market-priced session reads the same but says so") {
    parsed(Paste.replace("Loot Type: Leader", "Loot Type: Market")).lootType shouldBe "Market"
  }

  // --- refusing -----------------------------------------------------------

  test("anything that isn't an analyser is refused, quoting what was pasted") {
    val problem = refusal("how do i split loot")
    problem should include("Session data:")
    problem should include("how do i split loot")
  }

  test("an empty box is refused") {
    HuntAnalyser.parse("   \n  ").isLeft shouldBe true
  }

  test("a header with no Balance line is refused rather than split as zero") {
    val without = Paste.split("\n").filterNot(_ == "Balance: 9,005,562").mkString("\n")
    refusal(without) should include("Balance:")
  }

  /** Discord's paragraph box holds 4,000 characters, and a paste longer than that
   *  arrives cut off mid-block with nothing to say so. Splitting the members who
   *  did survive would hand back numbers that look right and are not. */
  test("a paste cut off mid-member is refused as cut off, naming who it stopped on") {
    val cut = Paste.substring(0, Paste.indexOf("\tBalance: -107,947"))
    val problem = refusal(cut)
    problem should include("Violent Beams")
    problem should include("4,000 characters")
  }

  test("a member missing a money line part-way up is refused as malformed") {
    val broken = Paste.split("\n").filterNot(_ == "\tBalance: 62,310").mkString("\n")
    val problem = refusal(broken)
    problem should include("Boss Jeremy")
    problem should include("without editing it")
  }

  // --- the split ----------------------------------------------------------

  test("the session runs for the header's real length, not the rounded label") {
    // 2h17m40s. The "02:17h" label rounds the 40 seconds away, and pricing the
    // loot against 2h17m00s overstates the hourly rate by about half a percent.
    parsed().durationSeconds shouldBe Some(8260L)
  }

  test("loot per hour") {
    parsed().lootPerHour shouldBe Some(6258575L)
  }

  test("individual balance floors the odd gold rather than inventing it") {
    // 9,005,562 / 4 is 2,251,390.5 exactly.
    parsed().individualBalance shouldBe 2251390L
  }

  test("damage shares, biggest first") {
    val shares = parsed().damageShares
    shares.map(_._1.name) shouldBe
      List("Violent Beams", "Neutrul The Wise", "Boss Jeremy", "The Wingga")
    shares.map(_._2).zip(List(37.0663, 25.9756, 21.1279, 15.8296)).foreach {
      case (actual, expected) => actual shouldBe (expected +- 0.001)
    }
  }

  test("healing shares, biggest first") {
    val shares = parsed().healingShares
    shares.map(_._1.name) shouldBe
      List("Neutrul The Wise", "The Wingga", "Violent Beams", "Boss Jeremy")
    shares.map(_._2).zip(List(45.3901, 23.5938, 15.9396, 15.0766)).foreach {
      case (actual, expected) => actual shouldBe (expected +- 0.001)
    }
  }

  test("nobody dealing damage produces no shares rather than a column of zeroes") {
    val quiet = parsed().copy(members = parsed().members.map(_.copy(damage = 0, healing = 0)))
    quiet.damageShares shouldBe empty
    quiet.healingShares shouldBe empty
  }

  /** The whole point of the feature: these are the three lines the leader types. */
  test("the transfers square the party up") {
    parsed().transfers shouldBe List(
      HuntTransfer("The Wingga", "Boss Jeremy", 2189080L),
      HuntTransfer("The Wingga", "Neutrul The Wise", 2999741L),
      HuntTransfer("The Wingga", "Violent Beams", 2359337L)
    )
  }

  test("a transfer reads as the command the game accepts, without separators") {
    parsed().transfers.head.command shouldBe "transfer 2189080 to Boss Jeremy"
  }

  test("everyone ends up on the individual balance, to within the floored gold") {
    val hunt = parsed()
    val moved = hunt.transfers.groupBy(_.to).view.mapValues(_.map(_.amount).sum).toMap
    hunt.members.filterNot(_.leader).foreach { member =>
      (member.balance + moved.getOrElse(member.name, 0L)) shouldBe hunt.individualBalance
    }
  }

  test("only those holding a surplus pay, and only once each") {
    parsed().transfersByPayer.map(_._1) shouldBe List("The Wingga")
  }

  test("a party already square needs no transfers") {
    val even = parsed().copy(
      balance = 400L,
      members = parsed().members.map(_.copy(balance = 100L))
    )
    even.transfers shouldBe empty
  }

  /** Two payers is rare — it needs two people to have looted more than their share
   *  — but the greedy match has to hold, and each payer has to get their own list. */
  test("two people in surplus each get their own transfers, in party order") {
    val member = (name: String, balance: Long) =>
      HuntMember(name, loot = 0, supplies = 0, balance = balance, damage = 0, healing = 0, leader = false)
    val hunt = parsed().copy(
      balance = 4000000L,
      members = List(
        member("Alpha Two", 2500000L),
        member("Beta Three", 1500000L),
        member("Gamma Four", -200000L),
        member("Delta Five", 200000L)
      )
    )
    hunt.individualBalance shouldBe 1000000L
    hunt.transfersByPayer shouldBe List(
      "Alpha Two" -> List(
        HuntTransfer("Alpha Two", "Gamma Four", 1200000L),
        HuntTransfer("Alpha Two", "Delta Five", 300000L)
      ),
      "Beta Three" -> List(
        HuntTransfer("Beta Three", "Delta Five", 500000L)
      )
    )
  }

  test("a solo session parses, and splits with nobody") {
    val solo = List(
      "Session data: From 2026-09-01, 21:12:00 to 2026-09-01, 22:12:00",
      "Session: 01:00h",
      "Loot Type: Leader",
      "Loot: 1,000,000",
      "Supplies: 400,000",
      "Balance: 600,000"
    ).mkString("\n")
    val hunt = parsed(solo)
    hunt.members shouldBe empty
    hunt.transfers shouldBe empty
    hunt.lootPerHour shouldBe Some(1000000L)
    hunt.individualBalance shouldBe 600000L
  }

  test("a header whose timestamps don't read costs the hourly rate and nothing else") {
    val hunt = parsed(Paste.replace("2026-09-01, 21:12:00", "yesterday evening"))
    hunt.from shouldBe empty
    hunt.lootPerHour shouldBe empty
    hunt.transfers should have size 3
  }

  // --- several hunts in one paste -----------------------------------------

  /** A real five-member session, the same numbers the party typed from. */
  private val Evening: String =
    List(
      "Session data: From 2026-10-06, 21:02:29 to 2026-10-06, 21:56:51",
      "Session: 00:54h",
      "Loot Type: Leader",
      "Loot: 9,234,027",
      "Supplies: 2,014,531",
      "Balance: 7,219,496",
      "Decay Skipuje Tury",
      "\tLoot: 2,171,915",
      "\tSupplies: 269,594",
      "\tBalance: 1,902,321",
      "\tDamage: 2,774,900",
      "\tHealing: 2,309,435",
      "Hansmann",
      "\tLoot: 1,310,742",
      "\tSupplies: 627,912",
      "\tBalance: 682,830",
      "\tDamage: 1,759,977",
      "\tHealing: 1,357,809",
      "Kubdrut",
      "\tLoot: 1,954,110",
      "\tSupplies: 196,116",
      "\tBalance: 1,757,994",
      "\tDamage: 2,520,525",
      "\tHealing: 1,624,116",
      "Oli Giga Spell",
      "\tLoot: 1,981,557",
      "\tSupplies: 389,679",
      "\tBalance: 1,591,878",
      "\tDamage: 2,511,505",
      "\tHealing: 346,760",
      "Simek Giga Maczuga (Leader)",
      "\tLoot: 1,815,703",
      "\tSupplies: 531,230",
      "\tBalance: 1,284,473",
      "\tDamage: 2,392,983",
      "\tHealing: 1,549,985"
    ).mkString("\n")

  private def hour(from: String, to: String, loot: Long, members: List[(String, Long)]): String = {
    val header = List(
      s"Session data: From $from to $to",
      "Session: 01:00h",
      "Loot Type: Leader",
      s"Loot: $loot",
      "Supplies: 0",
      s"Balance: $loot"
    )
    val body = members.flatMap { case (name, balance) =>
      List(name, s"\tLoot: $balance", "\tSupplies: 0", s"\tBalance: $balance", "\tDamage: 1", "\tHealing: 1")
    }
    (header ++ body).mkString("\n")
  }

  test("the five-man evening splits into the transfers they typed") {
    val hunt = parsed(Evening)
    hunt.members.map(_.name) shouldBe
      List("Decay Skipuje Tury", "Hansmann", "Kubdrut", "Oli Giga Spell", "Simek Giga Maczuga")
    hunt.balance shouldBe 7219496L
    hunt.individualBalance shouldBe 1443899L
    hunt.lootPerHour shouldBe Some(10190832L)
    hunt.transfersByPayer shouldBe List(
      "Decay Skipuje Tury" -> List(HuntTransfer("Decay Skipuje Tury", "Hansmann", 458422L)),
      "Kubdrut" -> List(
        HuntTransfer("Kubdrut", "Hansmann", 302647L),
        HuntTransfer("Kubdrut", "Simek Giga Maczuga", 11448L)
      ),
      "Oli Giga Spell" -> List(HuntTransfer("Oli Giga Spell", "Simek Giga Maczuga", 147978L))
    )
  }

  /** Afternoon + evening of 2026-10-06: Avalanche only in the first hunt,
   *  Decay from the second, Kubdrut only in the third. Combined they are six. */
  private val Afternoon: String =
    List(
      "Session data: From 2026-10-06, 16:03:56 to 2026-10-06, 17:04:40",
      "Session: 01:00h",
      "Loot Type: Leader",
      "Loot: 9,109,870",
      "Supplies: 1,834,650",
      "Balance: 7,275,220",
      "Avalanche Blond Loczek",
      "\tLoot: 128,030",
      "\tSupplies: 338,658",
      "\tBalance: -210,628",
      "\tDamage: 11,305,888",
      "\tHealing: 5,584,770",
      "Hansmann",
      "\tLoot: 2,807,480",
      "\tSupplies: 726,502",
      "\tBalance: 2,080,978",
      "\tDamage: 12,222,431",
      "\tHealing: 3,094,988",
      "Oli Giga Spell",
      "\tLoot: 992,108",
      "\tSupplies: 314,776",
      "\tBalance: 677,332",
      "\tDamage: 16,690,084",
      "\tHealing: 1,869,611",
      "Simek Giga Maczuga (Leader)",
      "\tLoot: 5,182,252",
      "\tSupplies: 454,714",
      "\tBalance: 4,727,538",
      "\tDamage: 6,767,022",
      "\tHealing: 2,693,927"
    ).mkString("\n")

  private val LateAfternoon: String =
    List(
      "Session data: From 2026-10-06, 17:09:04 to 2026-10-06, 19:11:20",
      "Session: 02:02h",
      "Loot Type: Leader",
      "Loot: 22,420,474",
      "Supplies: 4,493,643",
      "Balance: 17,926,831",
      "Decay Skipuje Tury",
      "\tLoot: 210,964",
      "\tSupplies: 1,181,801",
      "\tBalance: -970,837",
      "\tDamage: 30,256,194",
      "\tHealing: 13,685,337",
      "Hansmann",
      "\tLoot: 8,094,872",
      "\tSupplies: 1,599,563",
      "\tBalance: 6,495,309",
      "\tDamage: 28,793,086",
      "\tHealing: 5,687,432",
      "Oli Giga Spell",
      "\tLoot: 2,857,372",
      "\tSupplies: 446,577",
      "\tBalance: 2,410,795",
      "\tDamage: 37,865,526",
      "\tHealing: 3,695,401",
      "Simek Giga Maczuga (Leader)",
      "\tLoot: 11,257,266",
      "\tSupplies: 1,265,702",
      "\tBalance: 9,991,564",
      "\tDamage: 16,102,483",
      "\tHealing: 6,670,701"
    ).mkString("\n")

  /** The 21:02 session pasted first, then the two earlier ones, each copy
   *  carrying the mark the client actually puts on the clipboard. Read as one
   *  hunt those marks turn the later headers into members, the five-man balance
   *  is divided by fifteen, and somebody is told to transfer gold to themselves. */
  test("a later session whose header carries a clipboard mark stays its own hunt") {
    val pasted = "\uFEFF" + List(Evening, "\uFEFF" + LateAfternoon, "\u200E" + Afternoon).mkString("\n")
    val hunt = parsed(pasted)
    hunt.hunts shouldBe 3
    hunt.members.map(_.name) shouldBe List(
      "Decay Skipuje Tury", "Hansmann", "Kubdrut", "Oli Giga Spell",
      "Simek Giga Maczuga", "Avalanche Blond Loczek"
    )
    hunt.balance shouldBe 32421547L
    hunt.lootPerHour shouldBe Some(10304152L)
    hunt.transfers.exists(transfer => transfer.from == transfer.to) shouldBe false
    hunt.transfersByPayer shouldBe List(
      "Hansmann" -> List(
        HuntTransfer("Hansmann", "Decay Skipuje Tury", 1555180L),
        HuntTransfer("Hansmann", "Avalanche Blond Loczek", 262173L)
      ),
      "Kubdrut" -> List(
        HuntTransfer("Kubdrut", "Hansmann", 302647L),
        HuntTransfer("Kubdrut", "Simek Giga Maczuga", 11448L)
      ),
      "Simek Giga Maczuga" -> List(
        HuntTransfer("Simek Giga Maczuga", "Decay Skipuje Tury", 3438942L),
        HuntTransfer("Simek Giga Maczuga", "Oli Giga Spell", 3064407L),
        HuntTransfer("Simek Giga Maczuga", "Avalanche Blond Loczek", 1767260L)
      )
    )
  }

  test("three hunts with a rotating party settle each hunt, then net the transfers") {
    val hunt = parsed(List(Afternoon, LateAfternoon, Evening).mkString("\n"))
    hunt.hunts shouldBe 3
    hunt.members.map(_.name) shouldBe List(
      "Avalanche Blond Loczek", "Hansmann", "Oli Giga Spell",
      "Simek Giga Maczuga", "Decay Skipuje Tury", "Kubdrut"
    )
    hunt.loot shouldBe 40764371L
    hunt.supplies shouldBe 8342824L
    hunt.balance shouldBe 32421547L
    hunt.sessionLabel shouldBe "03:57h"
    hunt.lootPerHour shouldBe Some(10304152L)
    hunt.members.map(m => m.name -> m.balance) shouldBe List(
      "Avalanche Blond Loczek" -> -210628L,
      "Hansmann" -> 9259117L,
      "Oli Giga Spell" -> 4680005L,
      "Simek Giga Maczuga" -> 16003575L,
      "Decay Skipuje Tury" -> 931484L,
      "Kubdrut" -> 1757994L
    )
    hunt.transfersByPayer shouldBe List(
      "Hansmann" -> List(
        HuntTransfer("Hansmann", "Avalanche Blond Loczek", 262173L),
        HuntTransfer("Hansmann", "Decay Skipuje Tury", 1555180L)
      ),
      "Simek Giga Maczuga" -> List(
        HuntTransfer("Simek Giga Maczuga", "Avalanche Blond Loczek", 1767260L),
        HuntTransfer("Simek Giga Maczuga", "Oli Giga Spell", 3064407L),
        HuntTransfer("Simek Giga Maczuga", "Decay Skipuje Tury", 3438942L)
      ),
      "Kubdrut" -> List(
        HuntTransfer("Kubdrut", "Hansmann", 302647L),
        HuntTransfer("Kubdrut", "Simek Giga Maczuga", 11448L)
      )
    )
  }

  test("two hunts pasted back to back net opposite transfers into one command") {
    val first = hour("2026-10-06, 21:00:00", "2026-10-06, 22:00:00", 1000000L, List("Alpha" -> 700000L, "Beta" -> 300000L))
    val second = hour("2026-10-06, 23:00:00", "2026-10-07, 00:00:00", 1000000L, List("Alpha" -> 400000L, "Beta" -> 600000L))
    val hunt = parsed(first + "\n" + second)
    hunt.hunts shouldBe 2
    hunt.loot shouldBe 2000000L
    hunt.balance shouldBe 2000000L
    hunt.members.map(m => m.name -> m.balance) shouldBe List("Alpha" -> 1100000L, "Beta" -> 900000L)
    hunt.transfers shouldBe List(HuntTransfer("Alpha", "Beta", 100000L))
  }

  test("a name that only appears in one hunt is not given a share of the others") {
    val first = hour("2026-10-06, 21:00:00", "2026-10-06, 22:00:00", 1000000L, List("Alice" -> 800000L, "Bob" -> 200000L))
    val second = hour("2026-10-06, 23:00:00", "2026-10-07, 00:00:00", 1000000L, List("Bob" -> 800000L, "Carol" -> 200000L))
    val hunt = parsed(first + "\n" + second)
    hunt.transfers shouldBe List(
      HuntTransfer("Alice", "Bob", 300000L),
      HuntTransfer("Bob", "Carol", 300000L)
    )
  }

  test("combined loot per hour uses time spent hunting, not the break between hunts") {
    val first = hour("2026-10-06, 21:00:00", "2026-10-06, 22:00:00", 1000000L, List("Alpha" -> 1000000L))
    val second = hour("2026-10-06, 23:00:00", "2026-10-07, 00:00:00", 1000000L, List("Alpha" -> 1000000L))
    val hunt = parsed(first + "\n" + second)
    hunt.durationSeconds shouldBe Some(7200L)
    hunt.lootPerHour shouldBe Some(1000000L)
    hunt.sessionLabel shouldBe "02:00h"
  }

  test("eleven hunts are refused rather than silently dropping the rest") {
    val one = hour("2026-10-06, 10:00:00", "2026-10-06, 11:00:00", 100L, List("Solo" -> 100L))
    val problem = refusal(List.fill(11)(one).mkString("\n"))
    problem should include("11 hunts")
    problem should include("10")
  }

  test("a later hunt that does not parse names which one it was") {
    val first = hour("2026-10-06, 21:00:00", "2026-10-06, 22:00:00", 1000000L, List("Alpha" -> 1000000L))
    val broken = first.split("\n").filterNot(_ == "Balance: 1000000").mkString("\n")
    val problem = refusal(first + "\n" + broken)
    problem should include("Hunt 2")
    problem should include("Balance:")
  }
}
