package sufrin.regex

class IndexedCharSeq(chars: CharSequence) extends IndexedSeq[Char] {
  @inline def apply(i: Int): Char = chars.charAt(i)
  @inline def length: Int         = chars.length
  @inline def subSequence(start: Int, end: Int): IndexedSeq[Char] = chars.subSequence(start, end).toString
  }

  object IndexedCharSeq {
    def apply(string: String): IndexedSeq[Char] = string
    def apply(chars: CharSequence): IndexedSeq[Char] = new IndexedCharSeq(chars)
  }
