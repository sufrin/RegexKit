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

  def not: CharClass = CharClass(Predef.not(sat), s"[^$explain")

  def &&(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) && that.sat(ch), s"$explain&&${that.explain}")

  def ||(that: CharClass): CharClass = CharClass( (ch: Char) => this.sat(ch) || that.sat(ch), s"$explain${that.explain}")

  override def toString: String = s"[$explain]"
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
    'D' -> new PredefCharClass(not(_.isDigit), "\\D"),
    'w' -> new PredefCharClass(_.isLetterOrDigit, "\\w"),
    'W' -> new PredefCharClass(not(_.isLetterOrDigit), "\\W"),
    's' -> new PredefCharClass(_.isSpaceChar, "\\s"),
    'S' -> new PredefCharClass(not(_.isSpaceChar), "\\s"),
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


class Lexer(val text: CharSequence) extends Iterable[Lexeme] {

  def iterator: Iterator[Lexeme] = new Iterator[Lexeme] {
    private var chars = text.toString.toList

    def hasNext: Boolean = chars.nonEmpty

    def result(result: Lexeme, rest: List[Char]): Lexeme = {
      chars = rest; result
    }

    def result(result: Lexeme): Lexeme = {
      chars = chars.tail; result
    }

    def position(rest: List[Char]): Int = text.length - rest.length

    def next: Lexeme =
      chars match {
        // lexemes can be widely spaced, for clarity
        case ' ' :: rest => chars = rest; next
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
        case '\\' :: 'u'  :: a :: b :: c :: d :: rest if hexable (a,b,c,d) => result(Lit(hexer(a,b,c,d).toChar), rest)
        case '\\' :: 'u'  :: rest => SyntaxError(s"Invalid unicode escape at ${position(rest)} of $text")(Lit('\u0000'))
        case '\\' :: ch   :: rest => result(Predef(ch, { Lit(ch) }), rest)
        case '[' :: _ =>
          val (lexeme, rest_) = charClass(chars)
              result(lexeme, rest_)

        case other :: _ => result(Lit(other))
        case _ => End
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

    def primClass(chars: List[Char]): (CharClass, List[Char]) =
      chars match {
        case '^' :: rest =>
          val (lexeme, rest_) = charClass(rest)
          (lexeme.not, rest_)

        case '\\' :: '\\' :: rest =>
          (CharClass(_=='\\', "\\\\"), rest)

          // allow unicode escapes in character classes
        case '\\' :: 'u'  :: a :: b :: c :: d :: rest if hexable (a,b,c,d) =>
          val ch = hexer(a,b,c,d).toChar
          (CharClass(_==ch, s"$ch"), rest)

        // allow escapes in character classes
        case '\\' :: char :: rest  =>
          val ch = Predef.escape(char)
          (CharClass(_==ch, s"\\$char"), rest)

        case l :: '-' :: r :: rest => (new CharRange(l, r), rest)

        case '[' :: rest_ =>
          val (nextLex, rest__) = charClass(rest_)
          rest__ match {
            case ']':: rest___ => (nextLex, rest___)
            case ch :: next    => SyntaxError(s"Bad character class at ${position(rest__)}")(ZeroClass, rest__)
            case _             => (nextLex, rest__)
          }

        case '&' :: '&' :: '[' :: _    => (UnitClass, chars)
        case ']' :: rest               => (UnitClass, rest)
        case char :: rest              => (CharClass(_==char, s"$char"), rest)
        case List()                    => (ZeroClass, chars)
      }

    def charClass(chars: List[Char]): (CharClass, List[Char]) = {
      //println(s"charClass(${chars.mkString("")})")
      val initial = primClass(chars)
      var current: CharClass = initial._1
      var rest: List[Char]   = initial._2

        while (rest.nonEmpty) {
        //println(current, rest)
        rest match  {
          case '&' :: '&' :: '[' :: rest__ =>
            val (nextLex, rest_) = charClass(rest__)
                current = current && nextLex
                rest    = rest_
          case List()    =>
          case ']' :: rest_  =>
            rest = rest_
          case ch  :: _  =>
            val (nextLex, rest_) = primClass(rest)
            current = current || nextLex
            rest    = rest_
          case _ =>
        }
      }
      //println(s"charClass(${chars.mkString("")}) = ${(current,rest)}")
      (current, rest)
    }
  }
}
