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
    // Sequence of primitives, accumulating in reverse order
    // It would be natural to use a mutable stack here;
    // but the stock Scala stack behaves oddly
    var seq = List[Tree[Char]]()
    @inline def pop(): Tree[Char]         = seq match { case h::t => seq = t; h}
    @inline def push(t: Tree[Char]): Unit = seq = t :: seq
    @inline def mkSeq() = {
      val e = seq match {
        case List(e) => e
        case _       => Seq(seq.reverse)
      }
      if (tracing) println(s"=> ${e.source}")
      e
    }

    while (rd) {
      lexeme match {
        case End | Bar | Ket         =>  rd = false

        case Opt (nonGreedy)         =>  push (sufrin.regex.syntax.Opt (pop(), nonGreedy))
        case Plus(nonGreedy)         =>  push (sufrin.regex.syntax.Plus(pop(), nonGreedy))
        case Star(nonGreedy)         =>  push (sufrin.regex.syntax.Star(pop(), nonGreedy))

        case Lit(char)               => push (Literal(char))
        case CharClass(sat, explain) => push (Sat(sat, explain))

        case Bra =>
          val e = parseExpr()
          if (lexeme != Ket) SyntaxError(s"(...) malformed at $lexeme")(Literal('?'))
          push (Span(e, true))
      }
      if (rd) nextLexeme()
    }
    mkSeq()
  }


  private def parseExpr(): Tree[Char] =
    { nextLexeme()
      var e  = parseSeq()
      var rd = true
      while (rd) {
            lexeme match {
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
