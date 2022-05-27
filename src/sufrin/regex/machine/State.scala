package sufrin.regex.machine
import  sufrin.regex.machine.Program._

/** State of a single thread of a recogniser running the given program
 *  Two fibres running the same program are equal if they have the same pc.
 *  We '''never''' compare Fibres from different programs.
 *  The (start,end) indices of `Group`ed expressions recognised so far
 *  are saved as `groups`.
 */
class Fibre[T](program: Program[T], pc: Int, groups: Array[(Int, Int)]) {
  def ==(that: Fibre[T]): Boolean = pc==that.pc

  override def toString: String =
    s"Fibre($program, $pc, ${groups.mkString("[", ",", "]")})"

  // Stock overrides in case we use (hashed) collections
  override def equals(that: Any): Boolean =
    that match {
      case f: Fibre[T] => pc == f.pc
      case that        => require(false, "$this == $that is a badly-typed equality"); false
    }

  override def hashCode: Int = pc
}

/** A set of fibres for the program `program` */
class Fibres[T](program: Program[T]) {
  private val set = new collection.mutable.HashSet[Fibre[T]](program.length, 2.0) // but it shouldn't expand
  def clear(): Unit = set.clear()
  def += (fibre: Fibre[T]): Unit = set += fibre // no duplicates
  override def toString: String = s"Fibres($set)"
}

class State[T](groups: Array[(Int, Int)])
