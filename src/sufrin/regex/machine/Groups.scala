package sufrin.regex.machine

trait Groups {
  def setStart(group: Int, loc: Int): Groups
  def setEnd(group: Int, loc: Int):   Groups
}

class SimpleGroups(index: collection.immutable.ListMap[Int,Int]) extends Groups {
  def setStart(group: Int, loc: Int): Groups = new SimpleGroups(index + (2*group   ->  loc))
  def setEnd(group: Int, loc: Int): Groups   = new SimpleGroups(index + (1+2*group ->  loc))
  def getStart(group: Int): Option[Int] = index.get(2*group)
  def getEnd(group: Int):   Option[Int] = index.get(2*group+1)
  def apply(group: Int): Option[(Int, Int)] = getStart(group) match {
    case None => None
    case Some(start) => getEnd(group) match {
      case None => None
      case Some(end) => Some((start, end))
    }
  }

  override def toString: String = {
    val res = new StringBuilder()
    for { i <- 0 until index.size/2 }
      for { (s, e) <- apply(i) } res.addAll(s"$i:($s,$e) ")
    res.toString()
  }
    // index.mkString(",")

}

object Groups {
  val empty: SimpleGroups = new SimpleGroups(collection.immutable.ListMap.empty[Int,Int])
}
