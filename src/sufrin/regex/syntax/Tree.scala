package sufrin.regex.syntax
import sufrin.regex.machine
import sufrin.regex.machine.Program._

import scala.language.postfixOps

object Tree {
  implicit class SyntaxTree(val string: String) extends AnyVal {
    def ! : Tree[Char] = Seq(string.map(Literal(_)))
  }

  def ||[T](bs: Tree[T]*): Tree[T] = Branch(bs)

}

trait Tree[T]  {
  def compile(groups: Int, program: Builder[T]): Int
  def reversed: Tree[T]
  def source: String = this.toString

  /**  Can this expression start with `t`; taking into account nilpotent
   * subexpressions in it.
   */
  def canStartWith(t: T): Boolean

  /**
   * Literal explanations of the `canStartsWith` result
   */
  val canStart: List[String]

  /** true if this re ''can'' generate the empty sequence */
  def nilPotent: Boolean = false

  def | (r: Tree[T]): Tree[T]   = Alt(this, r)
  def ++(r: Tree[T]): Tree[T]   = Seq(List(this, r))
  def +   :  Tree[T]            = Plus[T](expr=this, short = false)
  def +?  : Tree[T]             = Plus[T](expr=this, short = true)
  def ?   : Tree[T]             = Opt[T](expr=this, short = false)
  def ??  : Tree[T]             = Opt[T](expr=this, short = true)
  def *   : Tree[T]             = Star[T](expr=this, short = false)
  def *?  : Tree[T]             = Star[T](expr=this, short = true)

  def compile(reverse: Boolean = false, showCode: Boolean = false): Program[T] = {
    val builder = new Builder[T]
    val (start, end) = (machine.Start[T](0), machine.End[T](0))
    val (ss, ee)     = if (reverse) (end, start) else (start, end)
    builder += ss
    if (reverse) reversed.compile(0, builder) else this.compile(0, builder)
    builder += ee
    builder += machine.Matched(-1)
    if (showCode) println(source)
    if (showCode) for (i <- 0 until builder.length) println(s"$i:\t${builder(i)}")
    builder.toProgram
  }

  def prettyPrint: Unit = sufrin.regex.PrettyPrint.prettyPrint(this)
}

case class Any[T]() extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Any
    groups
  }

  def reversed: Tree[T] = this
  override def source: String = "."

  def canStartWith(t:T)     = true
  val canStart: List[String] = List(".")
}

/**
 *   Surrounds `?` and `*` constructions, when the guard
 *   optimization is on.
 */
case class Guarded[T](expr: Tree[T]) extends Tree[T] with sufrin.regex.PrettyPrint.PrettyPrintable {
  override def prefix: String = "Guarded"
  def arity: Int = 2
  override def field(i: Int): (String, AnyRef) = i match {
    case 0 =>  ("expr", expr)
    case 1 =>  ("canStart", canStart)
  }

  def reversed: Tree[T] = Guarded(expr.reversed)

  override def source: String = s"${expr.source}$$$$$$"

  override val nilPotent: Boolean = expr.nilPotent
  val canStart: List[String] = expr.canStart
  def canStartWith(t:T): Boolean  = possible(t)
  lazy val possible: T=>Boolean   = expr.canStartWith(_)

  def compile(groups: Int, program: Builder[T]): Int = {
    val skip = machine.Lab[T](-1)
    program += machine.Guard(possible, skip, s"${canStart.mkString("CanStart(", " ", ")")}")
    expr.compile(groups, program)
    program.define(skip)
    groups
  }
}

case class Literal[T](v: T) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Lit(v)
    groups
  }
  def reversed: Tree[T] = this
  override def source: String = v.toString

  def canStartWith(t:T): Boolean = v==t
  val canStart: List[String] = List(v.toString)
}

case class Sat[T](sat: T => Boolean, explain: String) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.Sat(sat, explain)
    groups
  }
  def reversed: Tree[T] = this
  override def source: String = s"$explain"

  def canStartWith(t:T): Boolean = sat(t)
  val canStart: List[String] = List(explain)
}

case class Seq[T](seq: collection.Seq[Tree[T]])  extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    var g = groups
    for { expr <- seq } g = expr.compile(g, program)
    g
  }
  lazy val  reversed: Tree[T] = Seq(seq.reverse.map(_.reversed))
  override def source: String = seq.map(_.source).mkString("","","")
  override def ++(r: Tree[T]): Tree[T] = Seq(seq appended r)

  override val nilPotent: Boolean = seq.forall(_.nilPotent)

  def canStartWith(t: T) = possible(t)

  val canStart: List[String] = {
    val nilPotents : List[Tree[T]] = seq.takeWhile(_.nilPotent).toList
    if (nilPotents.isEmpty)
      seq.head.canStart
    else {
      val rest    : List[Tree[T]] = seq.dropWhile(_.nilPotent).toList
      val consider: List[Tree[T]]  =
        if (rest.isEmpty)
          nilPotents
        else
          rest.head :: nilPotents
      consider.foldRight(List.empty[String]) ( (t: Tree[T], r: List[String]) => t.canStart ++ r )
      }
  }

  /** precomputed predicate, evaluated once only */
  lazy val possible: T => Boolean = {
    val nilPotents = seq.takeWhile(_.nilPotent).toList
    if (nilPotents.isEmpty)
       seq.head.canStartWith(_)
    else {
       val rest = seq.dropWhile(_.nilPotent)
       val consider: List[Tree[T]] =
        if (rest.isEmpty)
          nilPotents
        else
          rest.head :: nilPotents
       consider.map { case t: Tree[T] => t.canStartWith(_) } .
               reduce { (p1: T => Boolean, p2: T => Boolean) => { (t: T) => p1(t) || p2(t) } }
    }
  }

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

  // Leftward priority is maintained. Is this a good idea?
  lazy val  reversed: Tree[T] = Alt(l.reversed, r.reversed)

  override def source: String = s"${l.source} | ${r.source}"
  def canStartWith(t:T): Boolean  = l.canStartWith(t) || r.canStartWith(t)
  override val nilPotent: Boolean = l.nilPotent || r.nilPotent
  val canStart: List[String] = l.canStart ++ r.canStart
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
    var maxGroups = 1
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

  lazy val reversed: Tree[T] = Branch(branches.reverse.map(_.reversed))

  override def compile(reversed: Boolean = false, showCode: Boolean = false): Program[T] = {
    val builder = new Builder[T]
    compile(0, builder)
    if (showCode) for (i <- 0 until builder.length) println(s"$i:\t${builder(i)}")
    builder.toProgram
  }

  override def source: String = branches.map(_.source).mkString("||(", ", ", ")")

  override def canStartWith(t: T): Boolean = true // not interesting yet at the top level

  val canStart: List[String] = {
    val starts = branches.map(_.canStart)
    starts.foldRight(List.empty[String])(_++_)
  }

}

case class Span[T](capture: Boolean=true, reverse: Boolean = false, expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    if (capture) {
      val (start, end) = (machine.Start[T](groups+1), machine.End[T](groups+1))
      val (ss, ee)     = if (reverse) (end, start) else (start, end)
      program += ss
      val groups_ = expr.compile(groups+1, program)
          program += ee
          groups_
    } else
      expr.compile(groups, program)
  }

  lazy val reversed: Tree[T] = Span(capture, !reverse, expr.reversed)

  override val nilPotent: Boolean = expr.nilPotent

  override def canStartWith(t: T): Boolean = expr.canStartWith(t)

  val canStart: List[String] = expr.canStart

  override def source: String = if (capture) s"(${expr.source})" else s"(?:${expr.source})"
}

/**
 *  Represents: `parseExpr?`` and the non-greedy `parseExpr??`
 *
 * {{{
 *   guard(this.canStartWith, lSkip)
 *   Fork(next, lEnd)
 *   next: parseExpr
 *   lEnd:
 *   lSkip:
 * }}}
 *
 *
 *  When `short` the fork spawns with the opposite priority
 *
 *  {{{ Fork(lEnd, next) }}}
 *
 */
case class Opt[T](short: Boolean=false, expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val next, lEnd = machine.Lab[T](-1)
    program += machine.Fork(if (short) List(next, lEnd) else List(lEnd, next))
    program.define(next)
    val groups_ = expr.compile(groups, program)
    program.define(lEnd)
    groups_
  }

  lazy val reversed: Tree[T] = Opt(short, expr.reversed)

  override val nilPotent: Boolean = true

  override def canStartWith(t: T): Boolean = expr.canStartWith(t)
  val canStart: List[String] = expr.canStart

  override def source: String = s"${expr.source}?"+(if (short) "?" else "")

}

/**
 *
 *  Represents: `parseExpr*` and the non-greedy `parseExpr*?`
 *
 *  {{{
 *    lStart:
 *      Fork(next, lEnd)
 *    next:
 *      parseExpr
 *      Jump(lStart)
 *    lEnd:
 *  }}}
 *
 *  When `short` the fork spawns with the opposite priority
 *
 *  {{{ Fork(lEnd, next) }}}
 */
case class Star[T](short: Boolean=false, expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val next, lStart, lEnd = machine.Lab[T](-1)
    program.define(lStart)
    program += machine.Fork(if (short) List(lEnd, next) else List(next, lEnd))
    program.define(next)
    val groups_ = expr.compile(groups, program)
    program += machine.Jump(lStart)
    program.define(lEnd)
    groups_
  }

  lazy val reversed: Tree[T] = Star(short, expr.reversed)

  override def source: String = s"${expr.source}*"+(if (short) "?" else "")

  override val nilPotent: Boolean = true

  def canStartWith(t: T): Boolean = expr.canStartWith(t)
  val canStart: List[String] = expr.canStart


}

/**
 *
 *  Represents: `parseExpr+` and the non-greedy `parseExpr+?`
 *
 *  {{{
 *   lStart:
 *      parseExpr
 *      Fork(lStart, next)
 *    next:
 *  }}}
 *
 *  When `short` the fork spawns with the opposite priority
 *
 *  {{{ Fork(next, lStart) }}}
 */
case class Plus[T](short: Boolean=false, expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val next, lStart = machine.Lab[T](-1)
    program.define(lStart)
    val groups_ = expr.compile(groups, program)
    program += machine.Fork(if (short) List(next, lStart) else List(lStart, next))
    program.define(next)
    groups_
  }

  lazy val  reversed: Tree[T] = Plus(short, expr.reversed)

  override def source: String = s"${expr.source}+"+(if (short) "?" else "")

  override val nilPotent: Boolean = expr.nilPotent

  def canStartWith(t: T): Boolean = expr.canStartWith(t)
  val canStart: List[String] = expr.canStart

}

/** A parseable anchor */
case class Anchor[T](left: Boolean) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += (if (left) machine.AtStart else machine.AtEnd)
    groups
  }

  lazy val  reversed: Tree[T] = Anchor(!left)
  override def source: String = if (left) "^" else "$"
  def canStartWith(t: T): Boolean = left // Not sure
  val canStart: List[String] = List.empty
}


/** An expression combinator for use in the DSL */
case class AnchorStart[T](expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    program += machine.AtStart
    expr.compile(groups, program)
  }

  lazy val  reversed: Tree[T] = AnchorEnd(expr.reversed)
  override def source: String = s"^${expr.source}"
  def canStartWith(t: T): Boolean = expr.canStartWith(t) // Not sure
  val canStart: List[String] = expr.canStart
  override val nilPotent: Boolean = expr.nilPotent

}

/** Matches at the start of the examined sequence */
case class AnchorEnd[T](expr: Tree[T]) extends Tree[T] {
  def compile(groups: Int, program: Builder[T]): Int = {
    val groups_ = expr.compile(groups, program)
    program += machine.AtEnd
    groups_
  }

  lazy val  reversed: Tree[T] = AnchorStart(expr.reversed)
  override def source: String = s"${expr.source}$$"
  def canStartWith(t: T): Boolean = expr.canStartWith(t) // Not sure
  val canStart: List[String] = expr.canStart
  override val nilPotent: Boolean = expr.nilPotent
}

