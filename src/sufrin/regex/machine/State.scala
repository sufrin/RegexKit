package sufrin.regex.machine
import  sufrin.regex.machine.Program._

/** State of a single thread of a recogniser running the given program
 *  Two fibres running the same program are equal if they have the same pc.
 *  We '''never''' compare Fibres from different programs.
 *  The (start,end) indices of `Group`ed expressions recognised so far
 *  are saved as `groups`.
 */
class Fibre[T](program: Program[T], val pc: Int, val groups: Groups) {

  def ==(that: Fibre[T]): Boolean = pc==that.pc

  override def toString: String =
    s"Fibre($pc, $groups)"

  // Stock overrides in case we use (hashed) collections
  override def equals(that: Any): Boolean =
    that match {
      case f: Fibre[T] => pc == f.pc
      case that        => require(false, "$this == $that is a badly-typed equality"); false
    }

  override def hashCode: Int = pc
}

/** A set of fibres for the program `program`, organised as a stack */
class FibreSet[T](program: Program[T]) {
  private val present   = Array.ofDim[Boolean](program.length)
  private val set       = Array.ofDim[Fibre[T]](program.length)
  private var size      = 0
  def nonEmpty: Boolean = size>0

  def clear(): Unit = {
     for { i<-0 until program.length } {
       set(i)     = null        // redundant
       present(i) = false
     }
     size = 0
  }

  locally { clear() }

  def addAt(pc: Int, fibre: => Fibre[T]): Unit = {
     if (!present(pc))  {
       set(size)   = fibre
       present(pc) = true
       size += 1
       println(s"add ${size-1}: ${set(size-1)}")
     } else {
       println(s"add: $pc was present")
     }
  }

  def fetchFibre(): Fibre[T] = {
    size -= 1
    val f = set(size)
    set(size) = null // for garbage collector
    present(f.pc) = false
    f
  }

  override def toString: String = (for { i <- 0 until size if present(i) } yield set(i)).mkString("\n ")
}


class State[T](program: Program[T], groups: Groups) {
  val l, r = new FibreSet(program)
  var (current, pending) = (l, r)
  @inline def swapFibreSets(): Unit = { val t = current; current = pending; pending = t; pending.clear() }

  /** Returns None if the executed instruction wasn't the `Success` at the end of the program;
   * otherwise returns the branch of the top-level alternation that
   * succeeded.
   */
  def execute(sourcePos: Int, t: T, pc: Int, groups: Groups): Option[(Int, Groups)] = {
    val result = program(pc).execute(sourcePos, t, pc, groups)
    println(s"$pc: ${program(pc)} ($sourcePos, $t, $pc) = $result")
    result match {
        case Stop =>
          None
        case Next(groups) =>
          pending.addAt(pc+1, { new Fibre[T](program, pc+1, groups) })
          None
        case Schedule(apc, groups) =>
          current.addAt(apc, { new Fibre[T](program, apc, groups) })
          None
        case Schedule2(pc1, pc2, groups) =>
          current.addAt(pc2, { new Fibre[T](program, pc2, groups) })
          current.addAt(pc1, { new Fibre[T](program, pc1, groups) })
          None
        case Success(branch: Int, groups: Groups) =>
          Some(branch, groups)
      }
  }

  def run(input: IndexedSeq[T], start: Int, end: Int): Option[(Int, Groups)] = {
    var pos = start
    var result: Option[(Int, Groups)] = None
    current.addAt(0, new Fibre(program, 0, groups))

    println(this)

    while (result.isEmpty && current.nonEmpty && pos<end) {
      var in = input(pos)
      println(s"$in@$pos")
      while (current.nonEmpty && result.isEmpty) {
        val fibre = current.fetchFibre()
        val groups = fibre.groups
        execute(pos, in, fibre.pc, groups) match {
          case None  =>
          case other =>
            result = other
        }
      }
      pos += 1
      swapFibreSets()
    }
    result
  }

  override def toString: String = s"State($groups)\n Current: $current\n Pending: $pending"
}
