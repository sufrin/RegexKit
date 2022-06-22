package sufrin.regex.machine
import sufrin.regex.machine.Program.Program

import scala.annotation.nowarn

/**
 *  Represents the state of a single thread of a running recogniser.
 *  Two fibres running the same program are equal if they have the same `pc`.
 *
 *  We '''never''' compare Fibres from different programs.
 *
 *  The `groups` represent information accumulated since the
 *  start of execution of the recogniser by this running fibre.
 */
case class Fibre[T](val pc: Int, groups: Groups) {

  def ==(that: Fibre[T]): Boolean = pc==that.pc

  override def toString: String = s"Fibre($pc, $groups)"

  /** '''Require:''' `that` '''must''' be a fibre
   * (a runtime error is thrown otherwise).
   *
   *  '''Returns:''' `this.pc==that.pc`
   */
  @nowarn override def equals(that: Any): Boolean =
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
  @inline def add(f: Fibre[T]): Unit = set.enqueue(f)
  @inline def fetch(): Fibre[T]      = set.dequeue()

  def clear(): Unit = {
    for { i<-0 until program.length } { present(i) = false }
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
      add(theFibre)
      // assert(theFibre.pc==pc, "addFibre($pc, $theFibre) -- $pc != ${theFibre.pc}")
      present(pc) = true
    }
  }

  def fetchFibre(): Fibre[T] = {
    val fibre = fetch()
    present(fibre.pc) = false
    fibre
  }

  override def toString: String = set.map(thread => s"${thread.pc}: ${thread.groups}").mkString("\n ")
  def repString: String = set.map(_.pc).mkString("[", ",", "]")
}
