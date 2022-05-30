package sufrin.regex.syntax
import sufrin.regex.machine
import sufrin.regex.machine.Program._

object Tree {
  implicit class SyntaxTree(val string: String) extends AnyVal {
    def ! : Tree[Char] = Seq(string.map(Literal(_)))
  }
  def ||[T](bs: Tree[T]*): Tree[T] = Branch(bs)
}

trait Tree[T]  {
  def compile(groups: Int, program: Builder[T]): Int
  def reversed: Tree[T]

  def |(r: Tree[T]): Tree[T]    = Alt(this, r)
  def +(r: Tree[T]): Tree[T]    = Seq(List(this, r))
  def ? : Tree[T]               = Opt[T](this)
  def * : Tree[T]               = Star[T](this)

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

case class Literal[T](v: T) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Lit(v)
    groups
  }
  def reversed: Tree[T] = this
}

case class Sat[T](sat: T => Boolean) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Sat(sat)
    groups
  }
  def reversed: Tree[T] = this
}

case class Seq[T](seq: collection.Seq[Tree[T]])  extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    var g = groups
    for { expr <- seq } g = expr.compile(g, program)
    g
  }
  def reversed: Tree[T] = Seq(seq.reverse.map(_.reversed))
}

case class Alt[T](l: Tree[T], r: Tree[T])  extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val lEnd, lAlt, lNext = machine.Lab[T](-1)
    program      += machine.Fork(List(lAlt, lNext))
    program.define(lNext)
    val groups_  = l.compile(groups, program)
    program      += machine.Jump(lEnd)
    program.define(lAlt)
    val groups__ = r.compile(groups_, program)
    program.define(lEnd)
    groups__
  }

  def reversed: Tree[T] = Alt(l.reversed, r.reversed)
}

/**
 * A `Branch` is effectively a many-branched `Alt` suitable for
 * use in components such as lexical scanners in which it
 * is necessary to know exactly which branch matches, and in which
 * the groups in each branch are addressed from 1.
 *
 * It cannot be nested within another Tree.
 * {{{
 * Save(0)
 * Fork(L0, L1, ..., Ln)
 *   L0: branches(0); Save(0); Matched(0)
 *   ...
 *   Ln: branches(n); Save(0); Matched(n)
 * }}}
 */

case class Branch[T](branches: collection.immutable.Seq[Tree[T]]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val lStarts   = branches.map { (tree: Tree[T]) =>  machine.Lab[T](-1) }
    var maxGroups = 0
    program  += machine.Start(0)
    program  += machine.Fork(lStarts)

    for { branch <- 0 until branches.length } {
      program.define(lStarts(branch))
      maxGroups = maxGroups max branches(branch).compile(1, program)
      program  += machine.End(0)
      program  += machine.Matched(branch)
    }

    maxGroups
  }
  def reversed: Tree[T] = Branch(branches.reverse.map(_.reversed))

  override def compile(showCode: Boolean = false): Program[T] = {
    val builder = new Builder[T]
    compile(0, builder)
    if (showCode) for (i <- 0 until builder.length) println(s"$i:\t${builder(i)}")
    builder.toProgram
  }
}



case class Group[T](expr: Tree[T], capture: Boolean=true) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    if (capture) {
      program += machine.Start(groups)
      val groups_ = expr.compile(groups + 1, program)
      program += machine.End(groups)
      groups_
    } else
      expr.compile(groups, program)
  }

  def reversed: Tree[T] = Group(expr.reversed, capture)
}

case class Opt[T](expr: Tree[T], preferNone: Boolean=false) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val next, over = machine.Lab[T](-1)
    program += machine.Fork(List(next, over))
    program.define(next)
    val groups_ = expr.compile(groups, program)
    program.define(over)
    groups_
  }

  def reversed: Tree[T] = Opt(expr.reversed, preferNone)
}

/**
 *  {{{
 *    lStart:
 *      Fork(next, lEnd)
 *    next:
 *      expr
 *      Jump(lStart)
 *    lEnd:
 *  }}}
 */
case class Star[T](expr: Tree[T], preferNone: Boolean=false) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val next, lStart, lEnd = machine.Lab[T](-1)
    program.define(lStart)
    program += machine.Fork(List(next, lEnd))
    program.define(next)
    val groups_ = expr.compile(groups, program)
    program += machine.Jump(lStart)
    program.define(lEnd)
    groups_
  }

  def reversed: Tree[T] = Opt(expr.reversed, preferNone)
}

/** Matches at the start of the examined sequence */
case class AnchorStart[T](expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.AtStart
    expr.compile(groups, program)
  }
  def reversed: Tree[T] = AnchorEnd(expr.reversed)
}

/** Matches at the start of the examined sequence */
case class AnchorEnd[T](expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val groups_ = expr.compile(groups, program)
    program += machine.AtEnd
    groups_
  }
  def reversed: Tree[T] = AnchorStart(expr.reversed)
}

