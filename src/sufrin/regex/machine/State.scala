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
   *  a `Matched` instruction executed during a `run`.
   *
   *  The matching algorithm keeps going after a successful
   *  match if there are prospects for a longer match.
   */
  private var lastResult: Result= None


  /**
   *  Instructions are treated homogeneously. All receive the same context for their
   *  execution: `program(pc).oneProgramStep(start, end, sourcePos, in, pc, groups)`.
   *
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
   *  In the first case it has scheduled any threads necessary to continuing the match
   *  or search.
   *
   *  '''NB:''' I gratefully acknowledge my colleague Mike Spivey's observation (based on
   *  the 1965 Thompson paper) that an appropriate technique for /searching/ rather than
   *  /matching/ is to spawn another thread at the origin of the program whenever the machine
   *  is about to shift to consideration of the next character. I had previously performed a
   *  match from successive starting locations.
   */
  def oneProgramStep(searching: Boolean, sourcePos: Int, in: T, pc: Int, groups: Groups): Result= {
    val continuation = program(pc).execute(start, end, sourcePos, in, pc, groups)
    if (this.traceSteps) println(s"$pc: ${program(pc)} ($sourcePos, $in, $pc) = $continuation")
    continuation match {
        case Stop =>
          // spawn a virgin fibre (see NB above)
          if (searching)
            pending.addFibre(0, { new Fibre[T](0, Groups.empty) })
          None
        case Next(groups) =>
          pending.addFibre(pc+1, { new Fibre[T](pc+1, groups) })
          if (searching) // spawn a virgin fibre (see NB above)
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

  def run(search: Boolean = true, tracePos: Boolean = false): Option[Match[T]] = {
    /*
     *  Invariant: sourcePos <= startPos <= end
     */
    var sourcePos      = start
    var result: Result = None

    /** Set `current` to the next NDA state */
    @inline def nextNDAState(in: T): Result= {
      var result: Result = None

      while (current.nonEmpty && result.isEmpty) {
        val fibre = current.fetchFibre()
        val groups = fibre.groups
        result = oneProgramStep(search, sourcePos, in, fibre.pc, groups)

        if (result.nonEmpty) lastResult = result
        // A candidate result appeared, but other threads are still active
        // and may match a longer sequence, so reject the candidate
        if (result.nonEmpty && pending.nonEmpty && sourcePos<end && !search)  result = None
      }
      // current.isEmpty || result.nonEmpty
      continue()
      result
    }

    current.addFibre(0, new Fibre(0, groups))
    sourcePos = start

    while (result.isEmpty && current.nonEmpty && sourcePos < end) {
      val in = input(sourcePos)
      if (tracePos) println(s"$in@$sourcePos")
      result = nextNDAState(in)
      sourcePos += 1
    }
    // result.nonEmpty || current.isEmpty || sourcePos==end

    if (traceSteps) println("Finally:")

    /* If `current.nonEmpty` then the transition to an accepting (or failing) state
     * still requires the execution of further ''housekeeping'' instructions
     */
    nextNDAState(arbitraryInput) match {
      case None    => result = lastResult
      case success => result = success
    }

    result match {
      case None => None
      case Some((index, groups)) => Some(new Match(input, index, groups))
     }
  }

  override def toString: String = s"State($groups)\n Current: $current\n Pending: $pending"
}

object State {
  /**
   * When successful, a `run` yields a result consisting of a
   *  branch-number (if the compiled program was a top-level
   *  `Branch`), together with the mapping from span indexes to
   *  the spans of the subpatterns that have been matched.
   */
  type Result = Option[(Int, Groups)]

  def apply[T](program: Program[T], groups: Groups, input: IndexedSeq[T], start: Int, end: Int, traceSteps: Boolean=false): State[T] =
      new State[T](program, groups, input, start, end, traceSteps)

}