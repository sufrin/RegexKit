package sufrin.regex.machine

trait Groups {
  def setStart(group: Int, loc: Int): Groups
  def setEnd(group: Int, loc: Int):   Groups
}

class SimpleGroups(index: collection.immutable.ListMap[Int,Int]) extends Groups {
  def setStart(group: Int, loc: Int): Groups = new SimpleGroups(index + (2*group   ->  loc))
  def setEnd(group: Int, loc: Int): Groups   = new SimpleGroups(index + (1+2*group ->  loc))
  override def toString: String = s"${index}"
  def getStart(group: Int): Option[Int] = index.get(2*group)
  def getEnd(group: Int):   Option[Int] = index.get(2*group)
  def apply(group: Int): Option[(Int, Int)] = getStart(group) match {
    case None => None
    case Some(start) => getEnd(group) match {
      case None => None
      case Some(end) => Some((start, end))
    }
  }
}

object Groups {
  val empty: SimpleGroups = new SimpleGroups(collection.immutable.ListMap.empty[Int,Int])
}
