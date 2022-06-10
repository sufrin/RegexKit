package sufrin.regex

import sufrin.regex.machine.Groups

/** Wrapper for a successful match */
trait Match[T] {
  val input:  IndexedSeq[T]
  val index:  Int
  val groups: Groups

  /**
   *   A view of the `i`'th group. At present this is implemented lazily,
   *   (as a slice) so the individual group is not "reified" completely, but
   *   left as a slice (a view of a sequence).
   *
   *   Reification can be forced by (many of) the `to`''XXX'' methods: `toArray,
   *   toList, toSet, toSeq, toIndexedSeq`.
   */
  def group(i: Int): Seq[T] = {
    groups(i) match {
      case None          => input.slice(0,0)
      case Some ((s, e)) => input.slice(s, e)
    }
  }

  /** The number of capturing groups of the match */
  def groupCount: Int = spans.size

  /** (start, end) positions of all groups in the match:
   *  -1 when a group is undefined
   */
  lazy val spans: Iterable[(Int, Int)] = groups.spans

  /** The entire matched subsequence */
  lazy val matched: Seq[T] = group(0)

  /** The start and end of the entire matched subsequence  */
  lazy val (start, end) = groups(0).getOrElse((-1, -1))

  /** The sequence of  matched subsequences -- including all subgroups  */
  lazy val allMatched: Seq[Seq[T]] =
    (for { (s, e) <- groups.spans } yield input.slice(s, e)).toSeq

  /**
   * An array consisting of all `String`s captured in this
   * `Match` -- the array is empty unless the subject/input
   * of the match was actually a `CharSequence`. In short,
   * this is relevant only for String/Charsequence matches.
   *
   * `toStrings(0)` is always the entire match.
   */
  lazy val toStrings: Array[String] = {
    input match {
      case in: sufrin.regex.IndexedCharSeq =>
        (for { (s, e) <- groups.spans } yield in.subSequence(s, e).toString).toArray
      case other =>
        Array()
    }
  }

  override def toString: String = s"/${allMatched.mkString("\"", "\", \"", "\"")}/ $groups ($index)=> "
}

object Match {
  /**
   * This supports integration of Scala pattern-matching with `Match`es; as in
   * {{{ aMatch match { case (a,b,c) => ... }}}
   */
  def unapplySeq[T](aMatch: Match[T]): Option[(Seq[Seq[T]])] = Some((aMatch.allMatched.tail)) // Matches the captured groups only
}
