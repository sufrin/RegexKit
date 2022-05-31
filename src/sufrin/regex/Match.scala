package sufrin.regex

import sufrin.regex.machine.Groups

/** Wrapper for a successful match */
class Match[T](val input: IndexedSeq[T], val index: Int, val groups: Groups) {

  /**
   *   A view of the `i`'th group. At present this is implemented lazily,
   *   so the group is not "reified" completely. Reification is forced by
   *   (many of) the `to`''XXX'' methods: `toArray, toList, toSet, toSeq, toIndexedSeq`.
   */
  def group(i: Int): Seq[T] = {
    groups(i) match {
      case None          => input.slice(0,0)
      case Some ((s, e)) => input.slice(s, e)
    }
  }

  /** The entire matched subsequence */
  lazy val matched: Seq[T] = group(0)

  /** The start and end of the entire matched subsequence  */
  lazy val (start, end) = groups(0).getOrElse((-1, -1))

  /** The entire sequence of  matched subsequences -- including all subgroups  */
  lazy val allMatched: Seq[Seq[T]] =
    (for { (s, e) <- groups.spans } yield input.slice(s, e)).toList

  override def toString: String = s"[${matched.length}] $index $groups => [$allMatched]"
}

object Match {
  def unapplySeq[T](aMatch: Match[T]): Option[(Int, Seq[Seq[T]])] = Some((aMatch.index, aMatch.allMatched))
}