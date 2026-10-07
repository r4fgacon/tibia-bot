package com.tibiabot.lootsplit

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

/** One party member's block in a Tibia party hunt analyser.
 *
 *  `balance` is `loot - supplies` as the client reported it rather than as this
 *  recomputes it: the client is the authority on what a member actually made, and
 *  a session where the two disagree is a session where the paste was edited.
 */
final case class HuntMember(
  name: String,
  loot: Long,
  supplies: Long,
  balance: Long,
  damage: Long,
  healing: Long,
  leader: Boolean
)

/** Who owes what to whom, in the form the game accepts: the payer types
 *  `transfer <amount> to <name>` into the party channel. */
final case class HuntTransfer(from: String, to: String, amount: Long) {
  /** Exactly what gets typed in-game — no thousands separators, because this is a
   *  command rather than a number to read. */
  def command: String = s"transfer $amount to $to"
}

/** A parsed party hunt analyser, plus everything derived from it.
 *
 *  Every derived value is computed here rather than in the embed, so the split can
 *  be tested without a Discord message — and so the numbers cannot drift between
 *  the DM's reply and `/lootsplit`'s.
 *
 *  ==Whole gold, floored==
 *  A four-way split of an odd balance does not come out even, and the game only
 *  moves whole gold. Every division here floors, and the leftover — at most one
 *  gold per member — stays with whoever was holding it. That is what the client
 *  itself does, and matching it is the point: a split that disagreed with the
 *  game's own arithmetic by a gold would be argued about.
 */
final case class HuntSession(
  from: Option[LocalDateTime],
  to: Option[LocalDateTime],
  /** The client's own `Session:` line, e.g. "02:17h". Kept verbatim rather than
   *  re-rendered from the timestamps: it is what the person pasting is looking at. */
  sessionLabel: String,
  lootType: String,
  loot: Long,
  supplies: Long,
  balance: Long,
  members: List[HuntMember],
  /** How many analyser pastes were folded into this session. One unless
   *  [[HuntSession.combine]] built it. */
  hunts: Int = 1,
  /** Sum of each hunt's real length, when this session is several hunts combined.
   *
   *  `from`/`to` then span the evening including the breaks, so pricing loot
   *  against that window would understate the hourly rate. Empty for a single
   *  hunt, which keeps using the header timestamps. */
  huntedSeconds: Option[Long] = None,
  /** Transfers already settled hunt-by-hunt and then netted, when this session is
   *  several hunts folded together. `None` means compute them from this session's
   *  members as a single split — the one-analyser case. */
  settledTransfers: Option[List[HuntTransfer]] = None,
  /** Extra supplies typed in after the analyser, as (name, amount) for whoever
   *  was given some. Empty unless [[withExtraSupplies]] built this session.
   *  Already folded into `supplies` and `balance` — kept so the reply can say
   *  why those two no longer match the paste. */
  extraSupplies: List[(String, Long)] = Nil
) {

  /** Seconds the party was actually hunting.
   *
   *  Not taken from the `Session:` label, which is rounded down to the minute: a
   *  2h17m40s hunt reads "02:17h" there, and pricing the loot against 2h17m00s
   *  overstates the hourly rate by half a percent. Empty when the header did not
   *  parse, which is the only reason the hourly figures can be missing.
   */
  def durationSeconds: Option[Long] =
    huntedSeconds.filter(_ > 0).orElse {
      for {
        start <- from
        end <- to
        seconds = ChronoUnit.SECONDS.between(start, end)
        if seconds > 0
      } yield seconds
    }

  def lootPerHour: Option[Long] = durationSeconds.map(seconds => Math.floorDiv(loot * 3600, seconds))

  /** What each member walks away with once the party is square — the number the
   *  transfers below are all working towards. */
  def individualBalance: Long =
    if (members.isEmpty) balance else Math.floorDiv(balance, members.size)

  /** Add out-of-analyser supplies — imbues, a prism, anything the client did not
   *  count. Each amount lines up with [[members]]. That player's supplies go up
   *  and their balance down by the same gold, and the party's supplies and
   *  balance move with them. Loot is untouched: nothing was looted, it was spent.
   *
   *  Amounts past the party are dropped, missing ones are zero, and a list of
   *  zeroes is the same session. A negative is not an expense; it is ignored
   *  rather than refunded, because the form that collects these rejects a minus
   *  before it gets here and a stray one should not invert the split. */
  def withExtraSupplies(amounts: Seq[Long]): HuntSession = {
    val extras = amounts.map(amount => math.max(amount, 0L)).padTo(members.size, 0L).take(members.size).toList
    if (extras.forall(_ == 0L)) this
    else {
      val updated = members.zip(extras).map { case (member, extra) =>
        member.copy(supplies = member.supplies + extra, balance = member.balance - extra)
      }
      val added = extras.sum
      copy(
        supplies = supplies + added,
        balance = balance - added,
        members = updated,
        extraSupplies = updated.zip(extras).collect { case (member, extra) if extra > 0L => member.name -> extra }
      )
    }
  }

  def totalDamage: Long = members.map(_.damage).sum

  def totalHealing: Long = members.map(_.healing).sum

  /** Each member's share of the party's damage, as a percentage, biggest first.
   *
   *  Empty when nobody dealt any — a fishing or a shopping session — rather than a
   *  list of zeroes, so the caller can leave the field off entirely instead of
   *  printing a column that says nothing. `sortBy` is stable, so members who dealt
   *  exactly the same stay in the order they were pasted in.
   */
  def damageShares: List[(HuntMember, Double)] = shares(_.damage, totalDamage)

  def healingShares: List[(HuntMember, Double)] = shares(_.healing, totalHealing)

  private def shares(of: HuntMember => Long, total: Long): List[(HuntMember, Double)] =
    if (total <= 0) Nil
    else members.map(member => member -> (of(member) * 100.0 / total)).sortBy(-_._2)

  /** What one member is owed (positive) or must pay out (negative), in whole gold.
   *
   *  `floorDiv` of the scaled-up difference rather than two separate divisions:
   *  computing `individualBalance - member.balance` from the already-floored
   *  individual figure loses the half-gold that the real split carries, and with
   *  four members that is enough to move every transfer by one.
   */
  private def owed(member: HuntMember): Long =
    Math.floorDiv(balance - members.size * member.balance, members.size)

  /** The transfers that square the party up, grouped in the order the members were
   *  pasted — which is the order the client lists them, and so the order the person
   *  reading recognises.
   *
   *  Greedy rather than optimal: each payer covers the members still short, in turn,
   *  until their surplus runs out. For the overwhelmingly common shape — one leader
   *  holding the loot and everybody else short — that is one transfer per member,
   *  which is already the fewest possible. A minimal-transfer solution for the rare
   *  many-payer session is a subset-sum problem, and the gain would be at most a
   *  line or two off a list nobody is paying per-line for.
   *
   *  Rounding leaves the payers owing up to one gold more in total than the
   *  receivers are short. That gold is simply never moved: no transfer is emitted
   *  for a surplus with nobody left to pay it to.
   */
  def transfers: List[HuntTransfer] =
    settledTransfers.getOrElse(splitThisHunt)

  /** The transfers that square this session's own members. Several hunts use
   *  [[HuntSession.netTransfers]] of this list instead, so a rotating party is
   *  not re-split as if everyone had been there the whole evening. */
  private def splitThisHunt: List[HuntTransfer] = {
    if (members.size < 2) Nil
    else {
      val payers = members.filter(member => owed(member) < 0)
      // Mutable only within this method: a fold threading both the remaining
      // shortfalls and the growing list reads worse than the two lines it saves.
      var shortfalls = members.collect { case member if owed(member) > 0 => member.name -> owed(member) }
      val settled = List.newBuilder[HuntTransfer]
      payers.foreach { payer =>
        var budget = -owed(payer)
        shortfalls = shortfalls.flatMap { case (name, short) =>
          val amount = math.min(budget, short)
          if (amount <= 0) Some(name -> short)
          else {
            settled += HuntTransfer(payer.name, name, amount)
            budget -= amount
            if (short > amount) Some(name -> (short - amount)) else None
          }
        }
      }
      settled.result()
    }
  }

  /** The transfers one payer has to type, in payer order. What the embed draws a
   *  field from — a payer with nothing to send never appears. */
  def transfersByPayer: List[(String, List[HuntTransfer])] = {
    val grouped = transfers.groupBy(_.from)
    members.map(_.name).distinct.flatMap(name => grouped.get(name).map(name -> _))
  }
}

object HuntSession {

  /** An evening of hunts: members matched by name so the embed can show totals,
   *  but each hunt is split on its own party and the transfers are then netted.
   *
   *  Splitting the summed balances as one session would give someone who sat out
   *  a hunt an equal share of hunts they were not on. Settling each analyser
   *  first, then adding `A pays B` across hunts (and cancelling `B pays A`), is
   *  the same commands they would have typed three times — one line each.
   *
   *  The hourly rate uses the time spent hunting, not the wall clock from the
   *  first start to the last end, so a break between hunts does not dilute it.
   */
  def combine(sessions: List[HuntSession]): HuntSession = sessions match {
    case Nil       => throw new IllegalArgumentException("combine on no hunts")
    case List(one) => one
    case many =>
      val order = many.flatMap(_.members.map(_.name)).distinct
      val byName = many.flatMap(_.members).groupBy(_.name)
      val members = order.map { name =>
        val appearances = byName(name)
        HuntMember(
          name = name,
          loot = appearances.map(_.loot).sum,
          supplies = appearances.map(_.supplies).sum,
          balance = appearances.map(_.balance).sum,
          damage = appearances.map(_.damage).sum,
          healing = appearances.map(_.healing).sum,
          leader = appearances.exists(_.leader)
        )
      }
      val hunted = {
        val lengths = many.map(_.durationSeconds)
        if (lengths.forall(_.isDefined)) Some(lengths.flatten.sum).filter(_ > 0) else None
      }
      HuntSession(
        from = many.flatMap(_.from).sortWith(_.isBefore(_)).headOption,
        to = many.flatMap(_.to).sortWith(_.isAfter(_)).headOption,
        sessionLabel = hunted.map(formatHours).getOrElse(""),
        lootType = if (many.exists(_.lootType.equalsIgnoreCase("Market"))) "Market" else many.head.lootType,
        loot = many.map(_.loot).sum,
        supplies = many.map(_.supplies).sum,
        balance = many.map(_.balance).sum,
        members = members,
        hunts = many.size,
        huntedSeconds = hunted,
        settledTransfers = Some(netTransfers(many.flatMap(_.transfers), order))
      )
  }

  /** One command per pair of names: amounts in the same direction add, opposite
   *  directions cancel. Order follows `memberOrder`, which is first-seen across
   *  the hunts — the same order the embed lists people in. */
  private[lootsplit] def netTransfers(all: List[HuntTransfer], memberOrder: List[String]): List[HuntTransfer] = {
    val orderIndex = memberOrder.zipWithIndex.toMap
    def idx(name: String): Int = orderIndex.getOrElse(name, Int.MaxValue)
    val net = scala.collection.mutable.Map.empty[(String, String), Long]
    all.foreach { transfer =>
      if (transfer.amount > 0 && transfer.from != transfer.to) {
        val aFirst = idx(transfer.from) < idx(transfer.to) ||
          (idx(transfer.from) == idx(transfer.to) && transfer.from < transfer.to)
        val canonical = if (aFirst) (transfer.from, transfer.to) else (transfer.to, transfer.from)
        val signed = if (canonical._1 == transfer.from) transfer.amount else -transfer.amount
        net(canonical) = net.getOrElse(canonical, 0L) + signed
      }
    }
    net.iterator.flatMap { case ((from, to), amount) =>
      if (amount > 0) Some(HuntTransfer(from, to, amount))
      else if (amount < 0) Some(HuntTransfer(to, from, -amount))
      else None
    }.toList.sortBy(transfer => (idx(transfer.from), idx(transfer.to)))
  }

  /** The client's `Session:` shape: zero-padded hours and minutes, then `h`. */
  def formatHours(seconds: Long): String = {
    val hours = seconds / 3600
    val minutes = (seconds % 3600) / 60
    f"$hours%02d:$minutes%02dh"
  }
}
