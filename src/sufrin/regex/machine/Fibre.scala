package sufrin.regex.machine
import sufrin.regex.machine.Program.Program

/**
 *  Represnts the state of a single thread of a running recogniser.
 *  Two fibres running the same program are equal if they have the same `pc`.
 *
 *  We '''never''' compare Fibres from different programs.
 *
 *  The `groups` represent information accumulated so far by
 *  this running fibre.
 */
case class Fibre[T](pc: Int, groups: Groups) {

  def ==(that: Fibre[T]): Boolean = pc==that.pc

  override def toString: String = s"Fibre($pc, $groups)"

  /** '''Require:''' `that` '''must''' be a fibre
   * (a runtime error is thrown otherwise).
   *
   *  '''Returns:''' `this.pc==that.pc`
   */
  override def equals(that: Any): Boolean =
    that match {
      case f: Fibre[T] => pc == f.pc
      case that        => require(false, "$this == $that is a badly-typed equality"); false
    }

  override def hashCode: Int = pc
}

/** A queue of fibres for the program `program`
 */
class FibreSet[T](program: Program[T]) {
  /** '''Invariant:'''
   *
   *  `present(pc)` iff there is a fibre `f` in `set` with `f.pc==pc`
   *
   *  `set.map(_.pc)` has no duplicates
   *
   *  `set.length` <= program.length
   */
  private val present   = Array.ofDim[Boolean](program.length)
  private val set       = new collection.mutable.Queue[Fibre[T]](10)  // Array.ofDim[Fibre[T]](program.length)
  @inline def nonEmpty: Boolean = set.nonEmpty

  def clear(): Unit = {
    for { i<-0 until program.length } { present(i) = false } // may be redundant: we only clear an empty queue
    set.clear()
  }

  locally { clear() }

  /**
   *  If there is not already such a fibre present,
   *  add a `Fibre` constructed by evaluating `fibre` .
   *  This '''must''' yield a fibre with the given `pc`.
   *
   *  The above condition cannot be checked in advance without
   *  perhaps forcing a `Fibre` to be constructed spuriously:
   *  an unnecessary cost.
   *
   *  A post-hoc check is not costly, but the number of places
   *  where `addFibre` is invoked is so small that the construction
   *  can be checked by eye.
   */
  def addFibre(pc: Int, fibre: => Fibre[T]): Unit = {
    if (!present(pc))  {
      val theFibre = fibre
      set.enqueue(theFibre)
      // assert(theFibre.pc==pc, "addFibre($pc, $theFibre) -- $pc != ${theFibre.pc}")
      present(pc) = true
    }
  }

  def fetchFibre(): Fibre[T] = {
    val fibre = set.dequeue()
    present(fibre.pc) = false
    fibre
  }

  override def toString: String = set.map(thread => s"${thread.pc}: ${thread.groups}").mkString("\n ")
}
