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

        case Opt (nonGreedy)         =>  push (sufrin.regex.syntax.Opt (nonGreedy, pop()))
        case Plus(nonGreedy)         =>  push (sufrin.regex.syntax.Plus(nonGreedy, pop()))
        case Star(nonGreedy)         =>  push (sufrin.regex.syntax.Star(nonGreedy, pop()))

        // for simplicity we don't enforce the end of the sequence at which an anchor appears
        case LeftAnchor              =>  push (sufrin.regex.syntax.Anchor(left=true))
        case RightAnchor             =>  push (sufrin.regex.syntax.Anchor(left=false))

        case Dot                     => push (sufrin.regex.syntax.Any())
        case Lit(char)               => push (Literal(char))
        case Sugar(tree)             => push(tree)
        case CharClass(sat, explain) => push (Sat(sat, s"[$explain]"))

        case Bra(capture) =>
          val e = parseExpr()
          if (lexeme != Ket) throw SyntaxError(s"( ${e.source} malformed at $lexeme when expecting ')'")
          push (Span(capture, reverse=false, e))
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

               case RightAnchor =>
                 nextLexeme()
                 e = sufrin.regex.syntax.AnchorEnd(e)

               case _ =>
                  rd = false
            }
            if (tracing) println(s"==> ${e.source}")
      }
      e
    }
}
