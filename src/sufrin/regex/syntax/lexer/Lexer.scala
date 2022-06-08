package sufrin.regex.syntax.lexer

class SyntaxError(message: String) extends Error(message) {
  def apply[T](t: T): T = { throw this; t }
}

object SyntaxError {
  def apply(message: String): SyntaxError = new SyntaxError(message)
}

sealed trait Lexeme {}
case object End extends Lexeme
case object Bra extends Lexeme
case object Ket extends Lexeme
case object Bar extends Lexeme
case object LeftAnchor  extends Lexeme
case object RightAnchor extends Lexeme
case object Dot         extends Lexeme
case class  Star(nonGreedy: Boolean) extends Lexeme
case class  Plus(nonGreedy: Boolean) extends Lexeme
case class  Opt (nonGreedy: Boolean) extends Lexeme
case class  Lit(char: Char) extends Lexeme

case class  CharClass(sat: Char => Boolean, explain: String) extends Lexeme {

  def not: CharClass = CharClass(Predef.not(sat), s"^$explain")

  def &&(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) && that.sat(ch), s"$explain&&[${that.explain}]")

  def ||(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) || that.sat(ch), s"$explain||${that.explain}")

  override def toString: String = s"[$explain]"

  /** iff this class is predefined by a criterion that
   * could include a boundary. For example, \D means any non-digit position,
   * not just any non-digit character: effectively equivalent to {{{([^d]|$|^)}}}
   */
   val includeBoundary: Boolean = false
}

class PredefCharClass (sat: Char => Boolean, explain: String) extends CharClass(sat, explain) {
  override def toString: String = s"$explain"
}

class CharRange(start: Char, end: Char) extends CharClass(  (ch:Char) => (start <= ch && ch <= end), s"$start-$end")

object UnitClass extends CharClass(  (_ : Char) => true, "") {
  override def &&(that: CharClass): CharClass = that
}

object ZeroClass extends CharClass(  (_ : Char) => false, "") {
  override def ||(that: CharClass): CharClass = that
}

object Predef {
  def not(pred: Char => Boolean): (Char=>Boolean) = ((ch:Char) => !(pred(ch)))
  private val table = collection.immutable.HashMap[Char,Lexeme](
    'd' -> new PredefCharClass(_.isDigit, "\\d"),
    'D' -> new PredefCharClass(not(_.isDigit), "\\D")         { override val includeBoundary: Boolean = true},
    'w' -> new PredefCharClass(_.isLetterOrDigit, "\\w"),
    'W' -> new PredefCharClass(not(_.isLetterOrDigit), "\\W") { override val includeBoundary: Boolean = true},
    's' -> new PredefCharClass(_.isSpaceChar, "\\s"),
    'S' -> new PredefCharClass(not(_.isSpaceChar), "\\S")     { override val includeBoundary: Boolean = true},
    'n' -> Lit('\n'),     // newline
    't' -> Lit('\t'),     // tab
    'r' -> Lit('\r'),     // cr
    'a' -> Lit('\u0007'), // bel
    'f' -> Lit('\u000C'), // ff
    'e' -> Lit('\u001B')  // esc
  )
  // locally { println(table) }
  def apply(ch: Char, orElse: => Lexeme): Lexeme =
      table.getOrElse(ch, orElse)

  def escape(ch: Char): Char =
    table.get(ch) match {
      case Some(Lit(char)) => char
      case _               => ch
    }
}

object Nonce extends Lit('\u0000')

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
        case '(' :: _ => result(Bra)
        case ')' :: _ => result(Ket)
        case '|' :: _ => result(Bar)
        case '^' :: _ => result(LeftAnchor)
        case '$' :: _ => result(RightAnchor)
        case '.' :: _ => result(Dot)
        case '*' :: '?' :: rest => result(Star(true), rest)
        case '+' :: '?' :: rest => result(Plus(true), rest)
        case '?' :: '?' :: rest => result(Opt(true), rest)
        case '*' :: _ => result(Star(false))
        case '+' :: _ => result(Plus(false))
        case '?' :: _ => result(Opt(false))
        case '\\' :: 's'  :: rest => result(Lit(' '), rest)
        case '\\' :: '\\' :: rest => result(Lit('\\'), rest)
        case '\\' :: 'u'  :: a :: b :: c :: d :: rest if hexable (a,b,c,d) => 
              result(Lit(hexer(a,b,c,d).toChar), rest)
        case '\\' :: 'u'  :: rest => 
              SyntaxError(s"Invalid unicode escape at $position")(Nonce)
        case '\\' :: ch   :: rest => result(Predef(ch, { Lit(ch) }), rest)
        case '['  :: rest =>
             val pos = position
             shift(rest)
             val next = charClass()
             chars match {
               case ']' :: rest => shift(rest); next
               case _           => SyntaxError(s"Unterminated character class starts at $pos")(next)
             }

        case ']' :: _   =>  SyntaxError(s"Stray unquoted ']' at $position")(Nonce)

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
          case _ => SyntaxError("Invalid hex digit: $ch")(0L)
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
          val ch = Predef.escape(char)
          result(CharClass(_ == ch, s"\\$char"), rest)

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
              SyntaxError(s"Unterminated character class starts at $position")(ZeroClass)
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
              SyntaxError(s"Unterminated character class starts at ${pos}")(ZeroClass)
          }

        case l :: '-' :: r :: rest =>
          result(new CharRange(l, r), rest)

        // TODO: discriminate between strict and lenient class syntax
        case char :: rest if "]&" contains char => 
          SyntaxError(s"Improper character spec (stray unquoted '$char') at $position")(ZeroClass)

        case char :: rest  =>
          result(CharClass(_ == char, s"$char"), rest)

        case _ =>  SyntaxError(s"Unterminated character spec at $position")(ZeroClass)
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
