package sufrin.regex.machine

case class Lab[T](var loc: Int)

trait Instruction[T]

case class Monitor[T](run: () => Unit) extends Instruction[T]
case class Lit[T](value: T)           extends Instruction[T]
case class Sat[T](sat: T => Boolean)  extends Instruction[T]
case class Start[T](group: Int)       extends Instruction[T]
case class End[T]  (group: Int)       extends Instruction[T]
case class Jump[T] (label: Lab[T])       extends Instruction[T]
case class Fork[T] (label: Lab[T])       extends Instruction[T]
case class Matched[T](branch: Int)    extends Instruction[T]


