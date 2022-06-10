import sufrin.regex.Regex
import sufrin.regex.Regex.StringMatch

def show(matched: StringMatch): Unit = {
    println(matched.toStrings.mkString(", "))
}

val intPat = Regex("""(\d+)""")
intPat.suffixes("1234567", 0, 7)
intPat.suffixes("abc1234567")
intPat.findSuffix("abc1234567qrstu")


intPat.matches("1234567", 0)
intPat.matches("1234567")

intPat.matches("a1234567a")       foreach show
intPat.findPrefix("a1234567a")    foreach show

val realPat = Regex("""\d+(?:\.\d+  [eE]-?\d+ | [eE]-?\d+)""")

val subject = ((("*"*5)++"1234.567e-6a")*2 ++ "23E-5")*3

val `realPat.allPrefixes` = realPat.allPrefixes(subject) .
  map {
      case StringMatch(s) => s
  } . toList

val `realPat.allSuffixes.reverse` = realPat.allSuffixes(subject) .
  map {
    case StringMatch(s) => s
  } . toList . reverse

val `realPat.allPrefixes.span` = realPat.allPrefixes(subject) .
    toList .
    map ( _.span )

val `realPat.allSuffixes.span.reverse` = realPat.allSuffixes(subject) .
  toList .
  map ( _.span ) .
  reverse

val `realPat.tree.source`= realPat.tree.source
val `realPat.tree.reversed.source`= realPat.tree.reversed.source


val wordPat = Regex("""(\w+)\W""")
val wordsPat= Regex("""(?:\w+\W)+""")

wordsPat  findPrefix " ====    foobaz is best for you" foreach show
wordsPat  findPrefix " ====  +++ --- " foreach show

val urlPat = Regex("((\\w+):)?(//(?:[.\\w]+/?)+)")

urlPat.allPrefixes("http://www.microsoft.com/some/other/url/path span://foo.bar //wiggle.wogle/foo/ ") . map {
  case StringMatch(_, _, proto, addr) => s"(4)[proto=$proto, addr=$addr]"
  case StringMatch(_, proto, addr) => s"(3)[proto=$proto, addr=$addr]"
  case StringMatch(_, addr) => s"(2)[proto=NONE, addr=$addr]"
} .toList

urlPat.allSuffixes("http://www.microsoft.com/some/other/url/path span://foo.bar/x/y //wiggle.wogle/foo/ bar") . toList . map {
  case StringMatch(_, _, proto, addr) => s"(4)[proto=$proto, addr=$addr]"
  case StringMatch(_, proto, addr) => s"(3)[proto=$proto, addr=$addr]"
  case StringMatch(_, addr) => s"(2)[proto=NONE, addr=$addr]"
}


(wordPat allPrefixes " ====    foobaz is best for you") . toList .
  map (_.toStrings) .
  map (_.mkString("[", ", ", "]"))



wordPat.allPrefixes("any blood my friends") . toList . map  {
    case StringMatch(s, t, u) => t
    case StringMatch(s, t) => t
}

urlPat.allPrefixes ("http://www.microsoft.com/some/other/url/path") .toList . map {
    case StringMatch(s) => (s)
}


























