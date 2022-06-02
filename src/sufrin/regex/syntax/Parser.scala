package sufrin.regex.syntax

import sufrin.regex.syntax.lexer._


class Parser (val text: String, val tracing: Boolean = false)  {
  lazy val tree: Tree[Char] = parseExpr()

  private val lexer   = new Lexer(text)
  private val lexemes = lexer.iterator
  private var lexeme: Lexeme = _

  private def nextLexeme()  = {
    if (lexemes.hasNext) lexeme = lexemes.next() else lexeme = End
    if (tracing) println(lexeme)
  }

  private def hasNext = lexemes.hasNext

  override def toString: String = s"Parser($text)"

  private def parseSeq(): Tree[Char] = {
    var rd  = true
    val seq = new collection.mutable.ListBuffer[Tree[Char]]
    def reduce() = {
      val e = if (seq.size==1) seq(0) else Seq(seq.toList)
      if (tracing) println(s"=> ${e.source}")
      e
    }
    while (rd) {
      lexeme match {
        case Opt(_) | Plus(_) | Star(_) |
             End    | Bar     | Ket  => rd = false
        case Lit(char)               => seq += Literal(char)
        case CharClass(sat, explain) => seq += Sat(sat, explain)
        case Bra =>
          val e = parseExpr()
          if (lexeme == Ket) nextLexeme() else SyntaxError(s"(...) malformed at $lexeme")(Literal('?'))
          seq += Span(e, true)
      }
      if (rd) nextLexeme()
    }
    // the result may not need to be `Seq`ified
    reduce()
  }

  private def parseExpr(): Tree[Char] =
    { nextLexeme()
      var e  = parseSeq()
      var rd = true
      while (rd) {
            lexeme match {
              case Opt (nonGreedy) => e = sufrin.regex.syntax.Opt (e, nonGreedy); nextLexeme()
              case Plus(nonGreedy) => e = sufrin.regex.syntax.Plus(e, nonGreedy); nextLexeme()
              case Star(nonGreedy) => e = sufrin.regex.syntax.Star(e, nonGreedy); nextLexeme()
              case Bar =>
                  nextLexeme()
                  e = Alt(e, parseSeq())
               case _ =>
                  rd = false
            }
            if (tracing) println(s"==> ${e.source}")
      }
      e
    }
}
