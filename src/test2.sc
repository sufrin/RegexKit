import sufrin.regex.Regex
import sufrin.regex.Regex.StringMatch



def show(matched: StringMatch): Unit = {
    println(matched.toStrings.mkString(", "))
}

val intPat = Regex("""(\d+)""")
intPat.matchesSuffix("1234567", 0, 7)
intPat.matchesSuffix("abc1234567")
intPat.findSuffix("abc1234567qrstu")


intPat.matches("1234567", 0)
intPat.matches("1234567")


intPat.matches("a1234567a")       foreach show
intPat.find("a1234567a")          foreach show

val realPat = Regex("""\d+\.\d*""")
realPat.findAll((("*"*30)++"1234.567a")*10) . map {
    case StringMatch(s) => s
} . toList

realPat.findAll((("*"*10)++"1234.567a")*10) . toList . map ( _.span )

val wordPat = Regex("""(\w+)\W""")
val wordsPat= Regex("""(?:\w+\W)+""")

wordsPat  find " ====    foobaz is best for you" foreach show
wordsPat  find " ====  +++ --- " foreach show

val urlPat = Regex("[^:]+://(?:[.\\w]+/?)+")
//pprint(urlPat.tree)
urlPat find "http://www.microsoft.com/some/other/url/path" foreach show

(wordPat findAll " ====    foobaz is best for you") . toList .
  map (_.toStrings) .
  map (_.mkString("[", ", ", "]"))



wordPat.findAll("any blood my friends") . toList . map  {
    case StringMatch(s, t, u) => t
    case StringMatch(s, t) => t
}

urlPat.findAll ("http://www.microsoft.com/some/other/url/path") .toList . map {
    case StringMatch(s) => (s)
}


























