package sufrin.regex.machine


/**
 *   An applicative mapping from numbered groups to their start/end locations
 *
 *   '''Represents:''' the two finite mappings
 *
 *      `starts, ends: Int+>Int`
 */
trait Groups {
  /** '''Returns:''' `g` such that `g.start = this.start + { group -> loc }` */
  def withStart(group: Int, loc: Int): Groups

  /** '''Returns:''' `g` such that `g.end = this.end + { group -> loc }` */
  def withEnd(group: Int, loc: Int): Groups

  /** '''Returns:''' `Some(starts(group)) iff `group`` is defined at `starts` */
  def getStart(group: Int): Option[Int]

  /** '''Returns:''' `Some(ends(group)) iff `group` is defined at `ends` */
  def getEnd(group: Int):   Option[Int]

  /** '''Returns:''' `Some(starts(group), ends(group))` iff group is defined at `starts` and at `ends` */
  def apply(group: Int): Option[(Int, Int)]
}

/** A `Groups` representation using a single list-based mapping: `index: Int+>Int`
 *
 *  '''Abstraction invariant:'''
 *     `starts` is `index` domain-restricted to even elements of its domain
 *     `ends` is `index` domain-restricted to odd elements of its domain
 */
class ListGroups(index: collection.immutable.ListMap[Int,Int]) extends Groups {
  def withStart(group: Int, loc: Int): Groups = new ListGroups(index + (2*group   ->  loc))
  def withEnd(group: Int, loc: Int): Groups   = new ListGroups(index + (1+2*group ->  loc))

  def getStart(group: Int): Option[Int] = index.get(2*group)

  def getEnd(group: Int):   Option[Int] = index.get(2*group+1)

  private def startOrElse(group: Int, alt: String): String = getStart(group) match
  { case None    => alt
    case Some(n) => n.toString
  }

  private def endOrElse(group: Int, alt: String): String= getEnd(group) match
  { case None => alt
    case Some(n) => n.toString
  }

  def apply(group: Int): Option[(Int, Int)] = getStart(group) match {
    case None => None
    case Some(start) => getEnd(group) match {
      case None => None
      case Some(end) => Some((start, end))
    }
  }

  /** Human-readable representation of the entire mapping, with '?'
   *  for unset indices.
   */
  override def toString: String = {
    val res = new StringBuilder()
    for { i <- 0 until index.size/2 }
      { val s = startOrElse(i, "?")
        val e = endOrElse(i, "?")
        res.addAll(s"$i:($s,$e) ")
      }
    res.toString()
  }
}

object Groups {
  val empty: ListGroups = new ListGroups(collection.immutable.ListMap.empty[Int,Int])
}
