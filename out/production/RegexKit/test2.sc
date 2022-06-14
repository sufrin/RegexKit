import sufrin.regex.Regex.StringMatch
import sufrin.regex.{Regex, TestKit}
import TestKit._




val intPat = Regex("""(\d+)""")
intPat.suffixes("1234567", 0, 7)
intPat.suffixes("abc1234567")
intPat.findSuffix("abc1234567qrstu")


intPat.matches("1234567", 0)
intPat.matches("1234567")


"*** Expecting None". show()
intPat.matches("a1234567a")      . show()
intPat.findPrefix("a1234567a")   . show()

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

"*************** WORDS ******************" . show
val wordPat = Regex("""(\w+)\W""")
val wordsPat= Regex("""(?:\w+\W)+""")

wordsPat.findPrefix(" ====    foobaz is best for you") . show
wordsPat.findSuffix(" ====    foobaz is best for you") . show
wordPat.allPrefixes(" ====  xyzzy +++ --- fiddle dedee ") . toList . show
wordPat.allSuffixes(" ====  xyzzy +++ --- fiddle dedee ") . toList . show

val urlPat = Regex("((\\w+):)?(//(?:[.\\w]+/?)+)")

urlPat.allPrefixes("http://www.sputum.com/some/other/url/path/ span://foo.bar //wiggle.wogle/foo bar") . show

urlPat.allSuffixes("http://www.sputum.com/some/other/url/path/ span://foo.bar/x/y //wiggle.wogle/foo/ bar") . show


(wordPat allPrefixes " ====    foobaz is best for you") . show


wordPat.allPrefixes(" the rain. ") . map {
  case StringMatch(_, s) => s
  case other => s"---$other---"
} . show

(wordPat.allPrefixes(TestKit.text)) . map {
  case StringMatch(_, s) => s
  case other => s"---$other---"
} . show



