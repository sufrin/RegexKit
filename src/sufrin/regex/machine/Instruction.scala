package sufrin.regex.machine

sealed trait Result { }

case object Stop                                            extends Result // kill the executing thread
case class Next(groups: Groups)                             extends Result // accept the current value and continue the executing thread
case class Schedule(pc: Int, groups: Groups)                extends Result // current closure computation
case class Schedule2(pc1: Int, pc2: Int, groups: Groups)    extends Result // current closure computation
case class Success(branch: Int, groups: Groups)             extends Result // which branch of the top-level alt


case class Lab[T](var loc: Int)

trait Instruction[T] {
  /** Execute the instruction and yield a continuation  */
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result
}

case class Monitor[T](run: () => Unit) extends Instruction[T] {
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result = Schedule(pc+1, groups)
}

case class Lit[T](value: T)           extends Instruction[T] {
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
      if (in==value) Next(groups) else Stop
}

case class Sat[T](sat: T => Boolean)  extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
      if (sat(in)) Next(groups) else Stop
}

case class Start[T](group: Int)       extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(pc+1, groups.setStart(group, sourcePos))
}

case class End[T]  (group: Int)       extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(pc+1, groups.setEnd(group, sourcePos))
}

case class Jump[T] (label: Lab[T])       extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(label.loc, groups)
}

case class Fork[T] (label: Lab[T])       extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule2(pc+1, label.loc, groups)
}

case class Matched[T](branch: Int)    extends Instruction[T]{
  def execute(sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Success(branch, groups)
}


