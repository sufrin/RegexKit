package sufrin.regex.machine
import  sufrin.regex.machine.Program._



class State[T](program: Program[T], groups: Groups, input: IndexedSeq[T], start: Int, end: Int, var traceSteps: Boolean=false) {
  /**
   * Current starting position of the subsequence being matched by `run`
   *
   * '''Inv''' `start <= startPos <= end`
   */
   var startPos = start

  private val l, r = new FibreSet(program)

  /**
   *   Each `Fibre` represents a trace of an DFA recogniser for the expression from which
   *   `program` was compiled.
   *
   *   The `current`` and `pending` `FibreSet`s each represent the state of an NDFA recogniser
   *   that has consumed the sequence of input between `startPos` and `sourcePos`.
   *
   *   After each phase of a match, these `FibreSet`s are swapped.
   */
  var (current, pending) = (l, r)

  /**
   *  The most recent successful result, if any, delivered by
   *  a `Matched` instruction executed during a `run`.
   *
   *  The matching algorithm keeps going after a successful
   *  match if there are ''prospects'' for a longer match.
   */
  var lastResult: Option[(Int, Groups)] = None

  @inline def swapFibreSets(): Unit = { val t = current; current = pending; pending = t; pending.clear() }

  /**
   *  Instructions are treated homogeneously right now, and all receive the context for their
   *  execution: `execute(start, end, sourcePos, in, pc, groups)`. This makes adding new kinds of instruction
   *  very straightforward -- but at the cost of needing an arbitrary in value to supply
   *  to the housekeeping instructions that are executed after the end of the input-proper
   *  has been reached.
   */
  private var arbitraryInput: T = _

  /** Returns `None` if the executed instruction wasn't the `Success` at the end of the program;
   * otherwise returns the index of the top-level branch that
   * succeeded, and the spans that captured starting and ending positions of
   */
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Option[(Int, Groups)] = {
    val result = program(pc).execute(start, end, sourcePos, in, pc, groups)
    if (this.traceSteps) println(s"$pc: ${program(pc)} ($sourcePos, $in, $pc) = $result")
    result match {
        case Stop =>
          None
        case Next(groups) =>
          pending.addFibre(pc+1, { new Fibre[T](pc+1, groups) })
          None
        case Schedule(apc, groups) =>
          current.addFibre(apc, { new Fibre[T](apc, groups) })
          None
        case Schedule2(pc1, pc2, groups) =>
          current.addFibre(pc1, { new Fibre[T](pc1, groups) })
          current.addFibre(pc2, { new Fibre[T](pc2, groups) })
          None
        case Success(branch: Int, groups: Groups) =>
          Some(branch, groups)
      }
  }

  def run(search: Boolean = true, tracePos: Boolean = false): Option[Match[T]] = {
    /*
     *  Invariant: sourcePos <= startPos <= end
     */
    var sourcePos                     = startPos
    var result: Option[(Int, Groups)] = None

    /** Set `current` to the next NDA state */
    @inline def nextNDAState(in: T): Option[(Int, Groups)] = {
      var result: Option[(Int, Groups)] = None
      while (current.nonEmpty && result.isEmpty) {
        val fibre = current.fetchFibre()
        val groups = fibre.groups
        result = execute(sourcePos, in, fibre.pc, groups)

        // A candidate result appeared, but other threads are still active
        // and may match a longer sequence, so reject the candidate
        if (result.nonEmpty) lastResult = result
        if (result.nonEmpty && pending.nonEmpty && sourcePos<end)  result = None
      }
      swapFibreSets()
      result
    }

    var searching = true

    while (searching) {

      current.addFibre(0, new Fibre(0, groups))
      sourcePos = startPos

      while (result.isEmpty && current.nonEmpty && sourcePos < end) {
        val in = input(sourcePos)
        if (tracePos) println(s"$in@$sourcePos")
        result = nextNDAState(in)
        sourcePos += 1
      }
      // Reached the end of the input, but there may still be some housekeeping
      // action for groups, anchors etc.
      if (tracePos) println("Cleanup:")

      // Cleaning up may still yield a result, if there is an anchor
      nextNDAState(arbitraryInput) match {
        case None    => result = lastResult
        case success => result = success
      }

      // if searching and no result, then try the match from the next position
      result match {
        case None => if (search && startPos <= end) startPos += 1 else searching = false
        case _    => searching = false
      }
    }

    result match {
      case None => None
      case Some((index, groups)) => Some(new Match(input, index, groups))
     }
  }

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

    lazy val matched: Seq[T] = group(0)

    override def toString: String = s"$matched[${matched.length}] $index $groups"
  }

  override def toString: String = s"State($groups)\n Current: $current\n Pending: $pending"
}
