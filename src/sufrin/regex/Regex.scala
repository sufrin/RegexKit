package sufrin.regex

import sufrin.regex.Util.IndexedCharSeq
import sufrin.regex.machine.{Groups, State}
import syntax.{Parser, Tree}

object Regex {
  def apply(source: String,   code: Boolean = false, trace: Boolean = false): Regex = new Regex(new Parser(source).tree, code, trace)
  def apply(tree: Tree[Char], code: Boolean, trace: Boolean): Regex = new Regex(tree, code, trace)
}

class Regex(val tree: Tree[Char], var code: Boolean, var trace: Boolean) {

  lazy val forwardCode = tree.compile(reverse = false, showCode = code)
  lazy val reverseCode = tree.compile(reverse = true,  showCode = code)

  override def toString: String = s"Regex(${tree.source})"

  def matchesPrefix(subject: CharSequence, from: Int = -1, to: Int = -1): Option[Match[Char]] = {
    val state = new State[Char](forwardCode, Groups.empty, IndexedCharSeq(subject), if (from>=0) from else 0, if (to>=0) to else subject.length , trace)
    state.run(search = false, trace)
  }

  def matches(subject: CharSequence, from: Int = -1, to: Int = -1): Option[Match[Char]] = {
    val state  = new State[Char](forwardCode, Groups.empty, IndexedCharSeq(subject), if (from>=0) from else 0, if (to>=0) to else subject.length, trace)
    var result = state.run(search = false, trace)
    result match {
      case Some(matched) => if (matched.end!=to) result = None
      case None =>
    }
    result
  }

  def findPrefix(subject: CharSequence, from: Int = -1, to: Int = -1): Option[Match[Char]] = {
    var first = if (from>=0) from else 0
    val last  = if (to>=0) to else subject.length
    var result: Option[Match[Char]] = None
    // horrible, quadratic, but necessary if we are capturing groups
    while (result.isEmpty && first < last) {
      result = new State[Char](forwardCode, Groups.empty, new IndexedCharSeq(subject), first, last, trace).run(search=false, trace)
      first += 1
    }
    result
  }

}

