package sufrin.regex.machine
import  sufrin.regex.Match
import  sufrin.regex.machine.Program._



class State[T](program: Program[T], groups: Groups, input: IndexedSeq[T], start: Int, end: Int, var traceSteps: Boolean=false) {
  import State._

  private val l, r = new FibreSet(program)

  /**
   *   Each `Fibre` represents a trace of an DFA recogniser for the expression from which
   *   `program` was compiled.
   *
   *   The `current`` and `pending` `FibreSet`s each represent the state of an NDFA recogniser
   *   that has consumed the sequence of input between `startPos` and `sourcePos`.
   *
   *   Each input is consumed by a major step in the algorithm, which is in two phases:
   *
   *      1. a ''housekeeping'' phase in which all instructions except for
   *      the matching instructions are executed.
   *
   *      2. a substantive step, in which
   *
   */
  private var (current, pending) = (l, r)
  /**
   *  Continue with the next phase.
   *
   * '''Pre:''' `current.isEmpty`
   *
   * '''Post:''' `pending.isEmpty`
   */
  @inline def continue(): Unit = { val t = current; current = pending; pending = t /*; pending.clear()*/ }

  /**
   *  The most recent successful result, if any, delivered by
   *  a `Matched` instruction executed during a `experiment`.
   *
   *  The matching algorithm keeps going after a successful
   *  match if there are prospects for a longer match.
   */
  private var lastResult: Result= None


  /**
   *  Instructions are treated homogeneously. All receive the same context for their
   *  execution at `program(pc)`, which is performed by:
   *  {{{
   *    oneProgramStep(start, end, sourcePos, in, pc, groups)
   *  }}}
   *  This makes adding new kinds of instruction very straightforward -- but at the cost of
   *  needing an arbitrary input value to supply to the housekeeping instructions executed
   *  after the end of the input-proper has been reached.
   */
  private var arbitraryInput: T = _

  /**
   *  A program step returns `None` if the executed instruction wasn't a `Matched` instruction
   *  that terminated the program with a continuation of `Success`; otherwise it transforms the
   *  `Success` into a (successful) `Result`.
   *
   *  In the first case it has scheduled any threads needed to continue the match
   *  or search.
   *
   *  '''Important Note:''' I gratefully acknowledge my colleague Mike Spivey's observation (based on
   *  the 1965 Thompson paper) that an appropriate technique for /searching/ rather than
   *  /matching/ is to spawn another thread at the origin of the program whenever the machine
   *  is about to shift to consideration of the next character.
   *  Although implemented here, the technique cannot be guaranteed to capture the content of
   *  nested groups appropriately during a search; indeed it can only ever be relied upon
   *  to find the end position of a match. One case in point is an expression of the form
   *  `(R1)+R2` when `R2` is empty, or has a prefix that may match a prefix of `R1`.
   *
   *  My judgment is that under most circumstances it is not worth trying to compose the above
   *  technique to find the endpoint of a match with a quadratic "match backwards" (from the endpoint)
   *  to capture groups accurately.
   */
  def oneProgramStep(searching: Boolean, logPos: Int, in: T, pc: Int, groups: Groups): Result = {
    val continuation = program(pc).execute(start, end, logPos, in, pc, groups)
    if (this.traceSteps) println(s"$pc: ${program(pc)} ($logPos, '$in', $pc) = $continuation")
    continuation match {
        case Stop =>
          // spawn a fresh fibre (see the important note above)
          if (searching && lastResult.isEmpty)
             pending.addFibre(0, { new Fibre[T](0, Groups.empty) })
          None
        case Next(groups) =>
          pending.addFibre(pc+1, { new Fibre[T](pc+1, groups) })
          // spawn a fresh fibre (see the important note above)
          if (searching && lastResult.isEmpty)
            pending.addFibre(0, { new Fibre[T](0, Groups.empty) })
          None
        case Schedule(apc, groups) =>
          current.addFibre(apc, { new Fibre[T](apc, groups) })
          None
        case ScheduleMany(pcs, groups) =>
          for { pc <- pcs } current.addFibre(pc, { new Fibre[T](pc, groups) })
          None
        case Success(branch: Int, groups: Groups) =>
          Some(branch, groups)
      }
  }

  def run(reversed: Boolean, search: Boolean = true, tracePos: Boolean = false): Option[Match[T]] = {
    /*
     *  Invariant: 0<=count<=limit
     */
    var sourcePos = if (reversed) end-1 else start
    var logPos    = if (reversed) end   else start
    var count     = 0
    val limit     = end-start
    val incPos    = if (reversed) -1 else +1
    var result: Result = None

    @inline def inspectNextInput(): Unit = {
      sourcePos += incPos
      logPos    += incPos
      count     += 1
    }

    /** Set `current` to the next NDA state */
    @inline def nextNDAState(in: T): Result = {
      var result: Result = None

      while (current.nonEmpty && result.isEmpty) {
        val fibre = current.fetchFibre()
        val groups = fibre.groups
        result = oneProgramStep(search, logPos, in, fibre.pc, groups)

        if (traceSteps) if (result.nonEmpty) println(s"        *** lastResult was $lastResult now $result")
        if (result.nonEmpty) lastResult = result
        // A candidate result appeared, but other threads are still active
        // and may match a longer sequence, so reject the candidate
        // if (traceSteps) if (!search && result.nonEmpty && pending.nonEmpty && count != limit) println(s"        *** result: $result => None\n        current: $current")
        if (!search && result.nonEmpty && pending.nonEmpty && count != limit) result = None
        if (traceSteps) { println(s"        *** C: ${current.repString}, P: ${pending.repString}") }
      }
      // current.isEmpty || result.nonEmpty
      continue()
      result
    }

    current.addFibre(0, new Fibre(0, groups))
    // sourcePos = if (reversed) end-1 else start

      while (result.isEmpty && current.nonEmpty && count != limit) {
        val in = input(sourcePos)
        if (tracePos) println(s"'$in'@$sourcePos")
        result = nextNDAState(in)
        inspectNextInput()
      }
      // result.nonEmpty || current.isEmpty || count == limit

      if (traceSteps) println(s"Finally: (result: $result, lastResult: $lastResult")

      var finalIn = arbitraryInput

      if (count != limit) {
        finalIn = input(sourcePos)
        if (tracePos) println(s"$finalIn@$sourcePos")
        inspectNextInput()
      }

     /* If `current.nonEmpty` then the transition to an accepting (or failing) state
      * still requires the execution of further ''housekeeping'' instructions
      */
      nextNDAState(finalIn) match {
        case None    => result = lastResult
        case success => result = success
      }


    // *******


    result match {
      case None => None
      case Some((index, groups)) => Some(wrap(input, index, groups))
     }
  }

  override def toString: String = s"State($groups)\n Current: $current\n Pending: $pending"

  def wrap(_input: IndexedSeq[T], _index: Int, _groups: Groups): Match[T]  = new Match[T] {
     val input  = _input
     val index  = _index
     val groups = _groups
  }
}

object State {
  /**
   * When successful, a `experiment` yields a result consisting of a
   *  branch-number (if the compiled program was a top-level
   *  `Branched`), together with the mapping from span indexes to
   *  the spans of the subpatterns that have been matched.
   */
  type Result = Option[(Int, Groups)]

  def apply[T](program: Program[T], groups: Groups, input: IndexedSeq[T], start: Int, end: Int, traceSteps: Boolean=false): State[T] =
      new State[T](program, groups, input, start, end, traceSteps)

}