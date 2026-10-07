package com.tibiabot.lootsplit

/** Amounts typed into the extra-expenses form, one per player on that page.
 *
 *  A blank box is zero — most of a party paid nothing on top. Anything else is
 *  read the way the analyser reads gold, commas and all, and a minus is refused
 *  rather than treated as a refund: this form only adds to supplies.
 */
object ExtraSupplies {

  /** The one session in `text`, or a sentence. Several hunts are a `/lootsplit`
   *  paste; this form lists players, and a combined evening has no single party
   *  to list. */
  def singleHunt(text: String): Either[String, HuntSession] =
    HuntAnalyser.parseAll(text).flatMap {
      case List(one) => Right(one)
      case many      => Left(s"That's ${many.size} hunts. This command takes one session — use /lootsplit for several.")
    }

  /** `raw` as gold for `name`, or a sentence that names them. Blank is zero. */
  def read(name: String, raw: String): Either[String, Long] = {
    val text = raw.trim
    if (text.isEmpty) Right(0L)
    else if (text.startsWith("-"))
      Left(s"**$name** is negative. Extra expenses are added on top of supplies, so leave a box blank instead.")
    else HuntAnalyser.goldAmount(text).filter(_ >= 0L).toRight(
      s"**$name** isn't a gold amount. Use a number like `50000`, or leave the box blank.")
  }

  /** One amount per name, in the same order. The first box that doesn't read
   *  fails the page — the rest are not applied either, so a typo doesn't move
   *  gold for the players above it. */
  def readPage(names: List[String], raws: List[String]): Either[String, List[Long]] = {
    val parsed = names.zip(raws.padTo(names.size, "")).map { case (name, raw) => read(name, raw) }
    parsed.collectFirst { case Left(problem) => problem } match {
      case Some(problem) => Left(problem)
      case None          => Right(parsed.collect { case Right(amount) => amount })
    }
  }
}
