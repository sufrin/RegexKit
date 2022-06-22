package sufrin.regex

import sufrin.regex.syntax.Branched

/**
 *  Forward and backward matching of fully-bracketed substrings
 */
object Brackets {
  /**
   *  Specification of a pair of brackets, together with a `Branched`
   *  pattern that matches either of them and indicates, with
   *  its index, which was matched.
   *
   */
  case class Specification(bra: Regex, ket: Regex, bracket: Regex) {
    def matchForward(subject: CharSequence, from: Int, to: Int): Option[Int] = Brackets.matchForward(this, subject, from, to)
    def matchBackward(subject: CharSequence, from: Int, to: Int): Option[Int] = Brackets.matchBackward(this, subject, from, to)
  }

  val BRA=0
  val KET=1

  /** Construct the specification of a pair of brackets  */
  def apply(bra: String, ket: String): Specification = {
    val bex: Regex = Regex(bra)
    val kex: Regex = Regex(ket)
    val bracket    = new Regex(Branched[Char](List(bex.tree, kex.tree)), showCode=false, trace=false)
    Specification(bex, kex, bracket)
  }

  /** Find the location of the end of the first fully-bracketed text between `from` and `to` */
  def matchForward(spec: Specification, input: CharSequence, from: Int, to: Int): Option[Int] =
  { val matches = spec.bracket.allPrefixes(input, from, to)
    var n      = 0
    var going  = true
    var result: Option[Int] = None
    while (going && matches.hasNext) {
      val pos = matches.next()
      pos.theMatch.index match {
        case `BRA` =>
            n += 1
        case `KET` =>
            n -= 1
            if (n==0) {
              going  = false
              result = Some(pos.end)
            }
      }
    }
    result
  }

  /** Find the location of the start of the last fully-bracketed text between `from` and `to` */
  def matchBackward(spec: Specification, input: CharSequence, from: Int, to: Int): Option[Int] =
  { val matches = spec.bracket.allSuffixes(input, from, to)
    var n      = 0
    var going  = true
    var result: Option[Int] = None
    while (going && matches.hasNext) {
      val pos = matches.next()
      pos.theMatch.index match {
        case `KET` =>
          n += 1
        case `BRA` =>
        n -= 1
        if (n==0) {
          going = false
          result = Some(pos.start)
        }
      }
    }
    result
  }

}
