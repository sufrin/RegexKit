package sufrin.regex.syntax.lexer

import sufrin.regex.syntax.Tree

/** Report of a parsing or lexical error.
 *  Thrown when applied.
 */
class SyntaxError(message: String) extends Error(message) {
  def apply[T](t: T): T = { throw this; t }
}

object SyntaxError {
  def apply(message: String): SyntaxError = new SyntaxError(message)
}

sealed trait Lexeme {}
case object End extends Lexeme
case class  Bra(capture: Boolean) extends Lexeme
case object Ket extends Lexeme
case object Bar extends Lexeme
case object Guard       extends Lexeme
case object LeftAnchor  extends Lexeme
case object RightAnchor extends Lexeme
case object Dot         extends Lexeme
case class  Star(nonGreedy: Boolean) extends Lexeme
case class  Plus(nonGreedy: Boolean) extends Lexeme
case class  Opt (nonGreedy: Boolean) extends Lexeme
case class  Lit(char: Char)          extends Lexeme
case class  Sugar(tree: Tree[Char])  extends Lexeme
/**
 *  Character classes expressed as `[...]` expressions are implemented as `Char` predicates, with human-readable `explain` fields
 *  that provide a handle on their source text.
 */
case class  CharClass(sat: Char => Boolean, explain: String) extends Lexeme {
  def not: CharClass = CharClass(Predef.not(sat), s"^$explain")
  def &&(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) && that.sat(ch), s"$explain&&[${that.explain}]")
  def ||(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) || that.sat(ch), s"$explain${that.explain}")
  override def toString: String = s"[$explain]"
}

/**
 *  Inbuilt character classes denoted by (eg `\D`, `\d`, etc) are implemented as `Char` predicates,
 *  and use their denoter as explanation.
 */
class PredefCharClass (sat: Char => Boolean, explain: String) extends CharClass(sat, explain) {
  override def toString: String = s"$explain"
}

/** a-z */
class CharRange(start: Char, end: Char) extends CharClass(  (ch:Char) => (start <= ch && ch <= end), s"$start-$end")

/** k */
class CharLit(char: Char, explain: String) extends CharClass( _ == char , explain)

/** All characters: identity of `&&` */
object ALLCHARACTERS extends CharClass((_ : Char) => true, "") {
  override def &&(that: CharClass): CharClass = that
}

/** No characters: identity of `||` */
object NOCHARACTERS extends CharClass((_ : Char) => false, "") {
  override def ||(that: CharClass): CharClass = that
}

/** Mapping to the predefined character classes and literals from their denoting letters. */
object Predef {
  
  def not(pred: Char => Boolean): (Char=>Boolean) = ((ch:Char) => !(pred(ch)))
  private val table = collection.immutable.HashMap[Char,CharClass](
    'd' -> new PredefCharClass(_.isDigit, "\\d"),
    'D' -> new PredefCharClass(not(_.isDigit), "\\D") ,
    'w' -> new PredefCharClass(_.isLetterOrDigit, "\\w"),
    'W' -> new PredefCharClass(not(_.isLetterOrDigit), "\\W"),
    's' -> new PredefCharClass(_.isSpaceChar, "\\s"),
    'S' -> new PredefCharClass(not(_.isSpaceChar), "\\S"),
    'n' -> new CharLit('\n',      "\\n"),     // newline
    't' -> new CharLit('\t',      "\\t"),     // tab
    'r' -> new CharLit('\r',      "\\r"),     // cr
    'a' -> new CharLit('\u0007',  "\\u0007"), // bel
    'f' -> new CharLit('\u000C',  "\\u000C"), // ff
    'e' -> new CharLit('\u001B',  "\\u001B")  // esc
  )
  // locally { println(table) }
  def apply(ch: Char, orElse: => CharClass): CharClass =
      table.getOrElse(ch, orElse)

  def apply(ch: Char): CharClass =
      table.get(ch).get

  /**
   *  A few character classes have different implementations at the
   *  top level of a `Regex` to their implementations within the
   *  bodies of `CharClass`es. These are treated as syntactic sugar for
   *  more complex `Regex`es.
   *
   *  For example, at the top level of a regex, the '''negated class'''
   *  symbols stand both for the character classes they describe and for
   *  either of the boundaries.
   *  {{{
   *    \D == [^\d] | ^ | $ == [\D] | ^ | $
   *    \W == [^\w] | ^ | $ == [\W] | ^ | $
   *    \S == [^\w] | ^ | $ == [\S] | ^ | $
   *  }}}
   *
   *  This is simply a convenience, so that (for example) a pattern that
   *  matches a ''word''  can be written -- more legibly -- as `\W(\w+)\W`
   *  and match words appearing anywhere, including at the right hand or
   *  the left-hand end of a search/match. The word itself is captured as
   *  group 1.
   *
   *  This little module supports the implementation detail.
   */
  object Sugared {
    import sufrin.regex.syntax._
    
    val anyAnchor: Tree[Char] = Alt[Char](Anchor(left=true), Anchor(left=false))

    def negatedPredef(char: Char): Tree[Char] = {
      val CharClass(sat, explain) = Predef(char)
      Span(capture=false, reverse=false, Alt[Char](sufrin.regex.syntax.Sat(sat, explain), anyAnchor))
    }

    private def mkSat(char: Char, explain: String): sufrin.regex.syntax.Tree[Char] = {
      sufrin.regex.syntax.Sat(_ == char, explain)
    }

    val crlf:               Tree[Char] = sufrin.regex.syntax.Seq[Char](List(mkSat('\r', "\\r"), mkSat('\n', "\\n")))
    val anyUnicodeLineEnd:  Tree[Char] = sufrin.regex.syntax.Sat[Char]("\u000A\u000B\u000C\u000D\u0085\u2028\u2029".contains(_), "\\R")
    val anyLineEnding:      Tree[Char] = Span(capture=false, reverse=false, Alt[Char](crlf, anyUnicodeLineEnd))
  }
  
  /**
   * Sugared constructs denoted by: \W, \D, \S, \R, $$
   */
  val sugarW:    Tree[Char] = Sugared.negatedPredef('W')
  val sugarD:    Tree[Char] = Sugared.negatedPredef('D')
  val sugarS:    Tree[Char] = Sugared.negatedPredef('S')
  val sugarR:    Tree[Char] = Sugared.anyLineEnding
  val anyAnchor: Tree[Char] = Sugared.anyAnchor


}

class Lexer(val text: CharSequence, tracing: Boolean = false) extends Iterable[Lexeme] {

  def iterator: Iterator[Lexeme] = new Iterator[Lexeme] {
    private var chars = text.toString.toList

    def hasNext: Boolean = chars.nonEmpty

    def result[T](result: T, rest: List[Char]): T = {
      chars = rest; result
    }

    def result[T](result: T): T = {
      chars = chars.tail; result
    }

    def shift(rest: List[Char]): Unit = chars = rest


    def position: String = s"#${text.length - chars.length} of $text"

    def next(): Lexeme =
      chars match {
        // lexemes can be widely spaced, for clarity
        case ' ' :: rest => chars = rest; next()
        case '(' :: '?' :: ':' :: rest  => result(Bra(capture=false), rest)
        case '(' :: _                   => result(Bra(capture=true))
        case ')' :: _                   => result(Ket)
        case '|' :: _                   => result(Bar)
        case '^' :: _                   => result(LeftAnchor)
        case '$' :: '$' :: '$' :: rest  => result(Guard, rest) // for experiments
        case '$' :: '$' :: rest         => result(Sugar(Predef.anyAnchor), rest)
        case '$' :: _                   => result(RightAnchor)
        case '.' :: _                   => result(Dot)
        case '*' :: '?' :: rest         => result(Star(true), rest)
        case '+' :: '?' :: rest         => result(Plus(true), rest)
        case '?' :: '?' :: rest         => result(Opt(true), rest)
        case '*' :: _                   => result(Star(false))
        case '+' :: _                   => result(Plus(false))
        case '?' :: _                   => result(Opt(false))

        case '\\' :: 's'  :: rest       => result(Predef('s'), rest)
        case '\\' :: 'R'  :: rest       => result(Sugar(Predef.sugarR), rest)
        case '\\' :: 'W'  :: rest       => result(Sugar(Predef.sugarW), rest)
        case '\\' :: 'D'  :: rest       => result(Sugar(Predef.sugarD), rest)
        case '\\' :: 'S'  :: rest       => result(Sugar(Predef.sugarS), rest)



        case '\\' :: '\\' :: rest       => result(Lit('\\'), rest)
        case '\\' :: 'u'  :: a :: b :: c :: d :: rest if hexable (a,b,c,d) =>
              result(Lit(hexer(a,b,c,d).toChar), rest)
        case '\\' :: 'u'  :: rest =>
              throw SyntaxError(s"Invalid unicode escape at $position")
        case '\\' :: ch   :: rest => result(Predef(ch, { new CharLit(ch, s"\\$ch") }), rest)
        case '['  :: rest =>
             val pos = position
             shift(rest)
             val next = charClass()
             chars match {
               case ']' :: rest => shift(rest); next
               case _           => throw SyntaxError(s"Unterminated character class starts at $pos")
             }

        case ']' :: _   =>  throw SyntaxError(s"Stray unquoted ']' at $position")

        case other :: _ =>  result(Lit(other))

        case _          => End
      }

    def hexable(hexChars: Char*): Boolean =
      hexChars forall (
        (ch: Char) =>
          (('0'<=ch) && (ch <= '9')) ||
          (('a'<=ch) && (ch <= 'f')) ||
          (('A'<=ch) && (ch <= 'F'))
      )

    def hexer(hexChars: Char*): Long = {
      hexChars.foldLeft(0L) {
        case (res: Long, ch) => res*16L + ch match {
          case _ if '0'<=ch && ch <= '9' => ch-'0'
          case _ if 'a'<=ch && ch <= 'f' => ch-'a' + 10L
          case _ if 'A'<=ch && ch <= 'F' => ch-'A' + 10L
          case _                         => throw SyntaxError("Invalid hex digit: $ch")
        }
      }
    }

    def primClass(): CharClass = {
      if (tracing) println(s"primClass @${chars.mkString("")}")
      val startPos = position
      chars match {
        // negated character within a class
        case '^' :: rest =>
          shift(rest)
          primClass().not

        case '\\' :: '\\' :: rest =>
          result(CharClass(_ == '\\', "\\\\"), rest)

        // allow unicode escapes in character classes
        case '\\' :: 'u' :: a :: b :: c :: d :: rest if hexable(a, b, c, d) =>
          val ch = hexer(a, b, c, d).toChar
          result(CharClass(_ == ch, s"$ch"), rest)

        // allow escapes in character classes
        case '\\' :: char :: rest =>
          result(Predef(char, { new CharLit(char, s"\\$char") }), rest)

        // negate the entire class that follows
        case '[' :: '^' :: rest =>
          val pos = position
          shift(rest)
          val next = charClass().not
          chars match {
            case ']' :: rest =>
              shift(rest)
              next
            case _ =>
              throw SyntaxError(s"Unterminated character class starts at $position")
          }

        case '[' :: rest =>
          val pos = position
          shift(rest)
          val next = charClass()
          chars match {
            case ']' :: rest =>
              shift(rest)
              next
            case _ =>
              throw SyntaxError(s"Unterminated character class starts at ${pos}")
          }

        case l :: '-' :: r :: rest =>
          result(new CharRange(l, r), rest)

        // TODO: discriminate between strict and lenient class syntax
        case char :: rest if "]&" contains char =>
          throw SyntaxError(s"Ill-formed character specification in class (stray unquoted '$char') at $position")

        case char :: rest  =>
          result(CharClass(_ == char, s"$char"), rest)

        case _ =>  throw SyntaxError(s"Unterminated character spec at $position")
      }

    }

    // <charclassbody>]
    def charClass(): CharClass = {
      //println(s"charClass(${chars.mkString("")})")
      var current: CharClass = primClass()
      var shifting = true
        while (shifting) {
          if (tracing) println(s"Charclass $current  @${chars.mkString("")}")
          chars match  {
            // end of this class
            case ']' :: _
            |    List() => shifting = false

            case '&' :: '&' :: rest =>
              shift(rest)
              val next = primClass()
              current = current && next

            case ch  :: _  =>
              val next = primClass()
              current = current || next

        }
      }
      //println(s"charClass(${chars.mkString("")}) = ${(current,rest)}")
      current
    }
  }
}
