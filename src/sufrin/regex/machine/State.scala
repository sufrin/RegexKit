package sufrin.regex.machine
import  sufrin.regex.machine.Program._



class State[T](program: Program[T], groups: Groups, input: IndexedSeq[T], start: Int, end: Int, var traceSteps: Boolean=false) {
  val l, r = new FibreSet(program)
  var (current, pending) = (l, r)

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

  def run(tracePos: Boolean = false): Option[(Int, Groups)] = {
    var pos                           = start
    var result: Option[(Int, Groups)] = None

    @inline def computeClosure(in: T): Option[(Int, Groups)] = {
      var result: Option[(Int, Groups)] = None
      while (current.nonEmpty && result.isEmpty) {
        //if (current.nonEmpty) println(s"Current: $current")
        val fibre = current.fetchFibre()
        val groups = fibre.groups
        result = execute(pos, in, fibre.pc, groups)
        // A candidate result appeared, but other threads are still active
        // and may match a longer sequence, so reject the candidate
        if (result.nonEmpty) lastResult = result
        if (result.nonEmpty && pending.nonEmpty && pos<end)  result = None
      }
      result
    }

    current.addFibre(0, new Fibre(0, groups))

    while (result.isEmpty && current.nonEmpty && pos<end) {
      val in = input(pos)
      if (tracePos) println(s"$in@$pos")
      result = computeClosure(in)
      //if (pending.nonEmpty) println(s"Pending: $pending")
      pos += 1
      swapFibreSets()
    }
    // Reached the end of the input, but there may still be some housekeeping
    // action for groups, anchors etc.
    if (tracePos) println("Cleanup:")
    // Cleaning up may still yield a result, if there is an anchor
    computeClosure(arbitraryInput) match {
      case None    => result = lastResult
      case success => result = success
    }

    result
  }

  override def toString: String = s"State($groups)\n Current: $current\n Pending: $pending"
}
