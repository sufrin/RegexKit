package sufrin.regex.machine

sealed trait Result { }

case object Stop                                            extends Result // kill the executing thread
case class Next(groups: Groups)                             extends Result // accept the current value and continue the executing thread
case class Schedule(pc: Int, groups: Groups)                extends Result // current closure computation
case class ScheduleMany(pcs: Seq[Int], groups: Groups)      extends Result // current closure computation
case class Success(branch: Int, groups: Groups)             extends Result // which branch of the top-level alt


case class Lab[T](var loc: Int)

trait Instruction[-T] {
  /** Execute the instruction and yield a continuation.
   *
   *  '''Requires'''
   *
   *      `start <= sourcePos <= end`
   *
   *      `sourcePos < end => in is a valid object of type T`
   */
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result
}

case object Any extends Instruction[Any] {
  def execute(start: Int, end: Int, sourcePos: Int, in: Any, pc: Int, groups: Groups): Result = Next(groups)
}

case class Monitor[T](run: () => Unit) extends Instruction[T] {
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result = Schedule(pc+1, groups)
}

case class Lit[T](value: T)           extends Instruction[T] {
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
      if (in==value) Next(groups) else Stop
}

case class Sat[T](sat: T => Boolean, explain: String)  extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
      if (sat(in)) Next(groups) else Stop
  override def toString: String = explain
}

case object AtStart   extends Instruction[Any]{
  def execute(start: Int, end: Int, sourcePos: Int, in: Any, pc: Int, groups: Groups): Result =
    if (sourcePos==start) Schedule(pc+1, groups) else Stop
}

case object AtEnd   extends Instruction[Any]{
  def execute(start: Int, end: Int, sourcePos: Int, in: Any, pc: Int, groups: Groups): Result =
    if (sourcePos==end) Schedule(pc+1, groups) else Stop
}

case object Anything extends Instruction[Any] {
  def execute(start: Int, end: Int, sourcePos: Int, in: Any, pc: Int, groups: Groups): Result =
    Next(groups)
}

case class Start[T](group: Int)       extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(pc+1, groups.withStart(group, sourcePos))
}

case class End[T]  (group: Int)       extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(pc+1, groups.withEnd(group, sourcePos))
}

case class Jump[T] (label: Lab[T])       extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Schedule(label.loc, groups)
  override def toString: String = s"-> ${label.loc}"
}

case class Fork[T] (labels: Seq[Lab[T]])       extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
    ScheduleMany(labels.map(_.loc), groups)
  override def toString: String = labels.map(_.loc).mkString(" =>"," =>", "")
}


case class Matched[T](branch: Int)    extends Instruction[T]{
  def execute(start: Int, end: Int, sourcePos: Int, in: T, pc: Int, groups: Groups): Result =
       Success(branch, groups)
}



