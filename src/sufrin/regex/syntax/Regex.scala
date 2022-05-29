package sufrin.regex.syntax
import sufrin.regex.machine
import sufrin.regex.machine.Program._

trait Regex[T]  {
  def compile(groups: Int, program: Builder[T]): Int
  def reversed: Regex[T]
  def |(r: Regex[T]): Regex[T] = Alt(this, r)
  def +(r: Regex[T]): Regex[T] = Seq(List(this, r))

  def compile(showCode: Boolean = false): Program[T] = {
    val builder = new Builder[T]
    builder += machine.Start(0)
    compile(1, builder)
    builder += machine.End(0)
    builder += machine.Matched(-1)
    if (showCode) for (i <- 0 until builder.length) println(s"$i:\t${builder(i)}")
    builder.toProgram
  }
}

case class Literal[T](v: T)                extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Lit(v)
    groups
  }
  def reversed: Regex[T] = this
}

case class Sat[T](sat: T => Boolean)       extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Sat(sat)
    groups
  }
  def reversed: Regex[T] = this
}

case class Seq[T](seq: collection.Seq[Regex[T]])  extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    var g = groups
    for { expr <- seq } g = expr.compile(g, program)
    g
  }
  def reversed: Regex[T] = Seq(seq.reverse.map(_.reversed))
}

case class Alt[T](l: Regex[T], r: Regex[T])  extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val lEnd, lAlt = machine.Lab[T](-1)
    program      += machine.Fork(lAlt)
    val groups_  = l.compile(groups, program)
    program      += machine.Jump(lEnd)
    program.define(lAlt)
    val groups__ = r.compile(groups_, program)
    program.define(lEnd)
    groups__
  }

  def reversed: Regex[T] = Alt(l.reversed, r.reversed)
}

/**
 * Compiled code for a TOP-LEVEL alternation
 *
 * Jump(lFork)
 *   L0: branches(0); Matched(0)
 *   ...
 *   Ln: branches(n); Matched(n)
 * lFork:
 *   Fork(l0)
 *   ...
 *   Fork(Ln-1)
 *   Jump(Ln)
 *
 */
case class Branches[T](branches: collection.Seq[Regex[T]]) extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    var groups_ = groups
    val lFork   = machine.Lab[T](-1)
    val lStarts  = for { expr <- branches } yield machine.Lab[T](-1)
    program     += machine.Jump(lFork)

    for { branch <- 0 until branches.length } {
      program.define(lStarts(branch))
      groups_ = branches(branch).compile(groups_, program)
      program += machine.Matched(branch)
    }

    program.define(lFork)
    for { lStart <- lStarts.init }
      program += machine.Fork(lStart)

    program += machine.Jump(lStarts.last)

    groups_
  }

  def reversed: Regex[T] = Branches(branches.reverse.map(_.reversed))

}
case class Group[T](expr: Regex[T], capture: Boolean) extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    if (capture) {
      program += machine.Start(groups)
      val groups_ = expr.compile(groups + 1, program)
          program += machine.End(groups)
          groups_ + 1
    } else
      expr.compile(groups, program)
  }

  def reversed: Regex[T] = Group(expr.reversed, capture)
}

/** Matches at the start of the examined sequence */
case class AnchorStart[T](expr: Regex[T]) extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.AtStart
    expr.compile(groups, program)
  }
  def reversed: Regex[T] = AnchorEnd(expr.reversed)
}

/** Matches at the start of the examined sequence */
case class AnchorEnd[T](expr: Regex[T]) extends Regex[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val groups_ = expr.compile(groups, program)
    program += machine.AtEnd
    groups_
  }
  def reversed: Regex[T] = AnchorStart(expr.reversed)
}

