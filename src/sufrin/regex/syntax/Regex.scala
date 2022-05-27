package sufrin.regex.syntax
import sufrin.regex.machine
import sufrin.regex.machine.Program.Builder

trait Regex[T]  {
  def compile(groups: Int, program: Builder[T]): Int
}

case class Lit[T](v: T)                extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Lit(v)
    groups
  }
}
case class Sat[T](sat: T => Boolean)       extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Sat(sat)
    groups
  }
}
case class Seq[T](seq: collection.Seq[Regex[T]])  extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    var g = groups
    for { expr <- seq } g = expr.compile(g, program)
    g
  }
}

case class Alt[T](l: Regex[T], r: Regex[T])  extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val lEnd, lAlt = machine.Lab[T](-1)
    program      += machine.Fork(lAlt)
    val groups_  = l.compile(groups, program)
    program      += machine.Matched(0)
    program      += machine.Jump(lEnd)
    lAlt.loc     = program.length
    val groups__ = r.compile(groups_, program)
    program      += machine.Matched(1)
    lEnd.loc     = program.length
    groups__
  }
}
case class Group[T](expr: Regex[T], capture: Boolean) extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Start(groups)
    expr.compile(groups+1, program)
    program += machine.End(groups)
    groups+1
  }
}

