package sufrin.regex

import sufrin.regex.machine.{Groups, State}
import sufrin.regex.syntax.{Parser, Tree}

import java.util.NoSuchElementException

object Regex {
  type CharMatch = Match[Char]

  /** Compile the guarded forms of `R*` and `R?` */
  var guarding: Boolean = false

  /** Value class representing a successful match */
  class StringMatch(val theMatch: Match[Char])  extends AnyVal {

    override def toString: String = theMatch.toStrings.mkString("StringMatch(", ",", ")")

    /**
     *   An iterable over the groups captured in the match
     */
    def toStrings: Seq[String] = theMatch.toStrings

    /** A list of the `(start,end)` of the groups captured during the match */
    def span: (Int,Int) = (start, end)

    /**
     *   The string captured in the group numbered `i`
     */
    def group(i: Int): String = theMatch.toStrings(i)

    /**
     *   An iterable over the groups captured in the match
     */
    def groups: Seq[String] = theMatch.toStrings

    def start:   Int    = theMatch.start
    def end:     Int    = theMatch.end

    /** The entire matched substring */
    def matched: String = group(0)

    @inline private def isDigit(c: Char): Boolean = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c)
    @inline private def index(c: Char):   Int     =  c match {
      case _ if '0' <= c && c <= '9' => c - '0'
      case _ if 'A' <= c && c <= 'Z' => c - 'A'
    }

    /**
     *  Replace all occurences of `"$$"`` in the template with `"$"`,
     *  and all occurences of `$i` (when `i` is a digit)
     *  with `group(i).getOrElse("")`. Uppercase roman letters `('A'..'Z')` are
     *  interpreted as digits denoting numbers starting at `0` -- so up to 26 groups
     *  can be substituted for.
     */
    def substitute(template: String): String = {
      val length = template.length
      val res = new StringBuilder(2 * length)
      var i = 0
      while (i < length) {
        template(i) match {
          case '$' if (i + 1 < length && template(i + 1) == '$') =>
            res.addOne('$')
            i += 2

          case '$' if (i + 1 < length && isDigit(template(i + 1))) =>
            res.append(groups(index(template(i + 1))))
            i += 2

          case ch =>
            res.addOne(ch)
            i += 1
        }
      }
      res.toString()
    }
  }

  object StringMatch {
    def apply(theMatch: Match[Char]): StringMatch = new StringMatch(theMatch)

    def unapplySeq(aMatch: StringMatch): Option[Seq[String]] = Some(aMatch.toStrings)
  }

  /**
   * Constructs the `Regex` specified by `source` providing that `source` conforms to
   * the (nearly-)standard notation for (extended) regular expressions. The exception
   * `SyntaxError` is thrown otherwise.
   *
   * @param source source of a regular expression
   * @param showCode print the compiled code of the automaton after compilation
   * @param trace trace the running automaton during searches/matches.
   */
  def apply(source: String,   showCode: Boolean = false, trace: Boolean = false): Regex =
      new Regex(new Parser(source).tree, showCode, trace)

  /**
   * @see Regex
   *
   * @param tree abstract syntax tree of a regular expression for matching `Char` sequences
   * @param showCode print the compiled code of the automaton after compilation
   * @param trace trace the running automaton during searches/matches.
   */
   def apply(tree: Tree[Char], showCode: Boolean, trace: Boolean): Regex =
       new Regex(tree, showCode, trace)

}

/**
 * A regular expression pattern with several methods for searching and matching
 * ''subjects'' (subsequences of a `CharSequence`) efficiently in either direction.
 *
 * When the first such method is invoked on a subject, the implementation generates instructions
 * for an abstract machine that simulates a nondeterministic finite automaton. These
 * instructions are generated only once; and any of the methods of the regex may
 * be re-used ad-lib.
 *
 * If the automaton accepts the subject, it returns {{{Some(match}}}
 * -- where `match: StringMatch` gives details of the way in which the
 * subject was accepted: in particular its starting and ending positions, and
 * the positions of any ''captured'' subgroups.
 *
 * @param tree parse tree for the regular expression (see the companion object `Regex` for another "constructor")
 * @param showCode print the compiled code after compilation
 * @param trace trace the running automaton during searches/matches.
 */

class Regex(val tree: Tree[Char], var showCode: Boolean, var trace: Boolean) {

  thisRegex =>

  import sufrin.regex.Regex.StringMatch

  /** The code for use in forward-matching operations: compiled on demand  */
  lazy val forwardCode = tree.compile(reverse = false, showCode = showCode)

  /** The code for use in reverse-matching operations: compiled on demand  */
  lazy val reverseCode = tree.compile(reverse = true,  showCode = showCode)

  override def toString: String = s"Regex(${tree.source})"

  /**
   * If the expression matches a prefix of {{{subject[from..to)}}} return `Some(theMatch)` otherwise return `None`
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def prefixes(subject: CharSequence, from: Int = -1, to: Int = -1): Option[StringMatch] = {
    val state = new State[Char](forwardCode, Groups.empty, IndexedCharSeq(subject), if (from>=0) from else 0, if (to>=0) to else subject.length , trace)
    for { theMatch <- state.run(reversed=false, search = false, trace) } yield StringMatch(theMatch)
  }

  /** Human-readable form of `forwardCode`, including instruction addresses. */
  def forwardCodeListing: String = (for {i<-0 until forwardCode.length} yield s"$i\t${forwardCode(i)}").mkString(s"${tree.source}\n", "\n", "\n")
  /** Human-readable form of `reverseCode`, including instruction addresses. */
  def reverseCodeListing: String = (for {i<-0 until reverseCode.length} yield s"$i\t${reverseCode(i)}").mkString(s"${tree.source}\n", "\n", "\n")

  /**
   * If the expression matches a prefix of {{{subject[from..to)}}} return `Some(theMatch)` otherwise return `None`
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def suffixes(subject: CharSequence, from: Int = -1, to: Int = -1): Option[StringMatch] = {
    val state = new State[Char](reverseCode, Groups.empty, IndexedCharSeq(subject), if (from>=0) from else 0, if (to>=0) to else subject.length , trace)
    for { theMatch <- state.run(reversed = true, search = false, trace) } yield StringMatch(theMatch)
  }

  /**
   * If the expression matches  {{{subject[from..to)}}} exactly return `Some(theMatch)` otherwise return `None`
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def matches(subject: CharSequence, from: Int = -1, to: Int = -1): Option[StringMatch] = {
    val start = if (from>=0) from else 0
    val end =   if (to>=0)   to   else subject.length
    val state  = new State[Char](forwardCode, Groups.empty, IndexedCharSeq(subject), start, end, trace)
    for { theMatch <- state.run(reversed = false, search = false, trace) if theMatch.end==end } yield StringMatch(theMatch)
  }

  /**
   * If there is a matching prefix of  {{{subject[from..to)}}} return `Some(theEarliestMatch)`
   * otherwise return `None`
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def findPrefix(subject: CharSequence, from: Int = -1, to: Int = -1): Option[StringMatch] = {
    var start = if (from>=0) from else 0
    val end   = if (to>=0)   to   else subject.length
    var result: Option[Match[Char]] = None
    // horrible, quadratic for a failing search, but appears to be necessary for the moment (see notes on `search` in `State.run`)
    while (result.isEmpty && start < end) {
      result = new State[Char](forwardCode, Groups.empty, IndexedCharSeq(subject), start, end, trace).run(reversed = false, search = false, trace)
      start += 1
    }
    for { theMatch <- result } yield StringMatch(theMatch)
  }
  /**
   * If there is a matching suffix of  {{{subject[from..to)}}} return `Some(theEarliestMatch)` otherwise return `None`
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def findSuffix(subject: CharSequence, from: Int = -1, to: Int = -1): Option[StringMatch] = {
    val start = if (from>=0) from else 0
    var end   = if (to>=0)   to   else subject.length
    var result: Option[Match[Char]] = None
    // horrible, quadratic for a failing search, but appears to be necessary for the moment (see notes on `search` in `State.run`)
    while (result.isEmpty && start < end) {
      result = new State[Char](reverseCode, Groups.empty, IndexedCharSeq(subject), start, end, trace).run(reversed = true, search = false, trace)
      end -= 1
    }
    for { theMatch <- result } yield StringMatch(theMatch)
  }

  /**
   * Return an iterator over the sequence of non-overlapping prefixes
   * within `subject[from..to)`.
   *
   * '''NB:''' The iteration is safe if (and only if) the `subject` is not changed
   * materially during the lifetime of the iterator; something that the
   * `CharSequence` interface does not guarantee.
   *
   * But it is possible to reify the string matches and their spans, etc, so that they
   * are subsequently independent of the state of the `subject`, and depend only on
   * its state between the start and end of the search.
   *
   * Here is a confected example:
   * {{{
   * val realPat = Regex("""\d+(?:\.\d*  [eE]-?\d+ | [eE]-?\d+)""")
   * realPat.allPrefixes(((("*"*30)++"34.567e-6a")*2++"3E-5")*3) .
   *   map {
   *     case StringMatch(s) => s.toFloat
   *   } . toList
   *
   *   realPat.allPrefixes(((("*"*30)++"34.567e-6a")*2++"3E-5")*3) .
   *     toList .
   *     map ( _.span )
   * }}}
   *
   * It yields
   * {{{
   *    List(0.001234567, 0.001234567, 2.3E-4,
   *         0.001234567, 0.001234567, 2.3E-4,
   *         0.001234567, 0.001234567, 2.3E-4)
   *}}}
   *
   *
   * '''Defaults'''
   * If `to` isn't supplied, then it is taken to be `subject.length`.
   * If neither `from` nor `to` appears, then the entire
   * `subject` is considered.
   */
  def allPrefixes(subject: CharSequence, _from: Int = -1, _to: Int = -1): Iterator[StringMatch] = new Iterator[StringMatch] {
    var first   = if (_from >=0) _from else 0
    val last    = if (_to  >=0)  _to   else subject.length
    var result: Option[StringMatch] = None
    var needsFind = true

    def hasNext: Boolean = {
      if (needsFind) { result = thisRegex.findPrefix (subject, first, last); needsFind = false }
      result.nonEmpty
    }

    def next(): StringMatch = {
      if (hasNext) {
        first = result.get.end
        needsFind = true
        result.get
      }
      else throw new NoSuchElementException(s"$thisRegex.allPrefixes iterator ran out")
    }
  }
  /**
   *  Return an iterator over the sequence of non-overlapping ''suffixes''
   * within `subject[from..to)`.
   *
   * @see allPrefixes
   *
   */
  def allSuffixes(subject: CharSequence, _from: Int = -1, _to: Int = -1): Iterator[StringMatch] = new Iterator[StringMatch] {
    val first   = if (_from >=0) _from else 0
    var last    = if (_to  >=0)  _to   else subject.length
    var result: Option[StringMatch] = None
    var needsFind = true

    def hasNext: Boolean = {
      if (needsFind) { result = thisRegex.findSuffix (subject, first, last); needsFind = false }
      result.nonEmpty
    }

    def next(): StringMatch = {
      if (hasNext) {
        last = result.get.start
        needsFind = true
        result.get
      }
      else throw new NoSuchElementException(s"$thisRegex.allSuffixes iterator ran out")
    }
  }

}

