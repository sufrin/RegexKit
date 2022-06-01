package sufrin.regex.syntax

object Parser {

}

trait Lexeme {}


class Lexer(text: CharSequence) extends Iterator[Lexeme] {
  var pos = 0
}
