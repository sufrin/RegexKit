import TestKit._
import sufrin.regex.Regex



val intPat = Regex("""(\d*\d)""")
intPat.tree.prettyPrint
intPat.forwardCodeListing.show()
intPat.reverseCodeListing.show()
val intPat1 = Regex("""(\d\d*)""")
intPat1.tree.prettyPrint
intPat1.forwardCodeListing.show()
intPat1.reverseCodeListing.show()

"Suffixes (all expecting 1234567)" . show()
intPat.suffixes("1234567", 0, 7)
intPat1.suffixes("1234567", 0, 7)
intPat.suffixes("abc1234567")
intPat.findSuffix("abc1234567qrstu")


"Matches at various places/sizes. Expecting 4567, 456" . show()
"*** Expecting 4567". show()
intPat.matches("1234567", 3) .show()
"*** Expecting 456". show()
intPat.matches("1234567", 3, 6) .show()


"*** Expecting None". show()
intPat.matches("a1234567a")      . show()
"*** Expecting 1234567". show()
intPat.findPrefix("a1234567a")   . show()

val realPat = Regex("""\d+(?: \.\d+  ([eE]-?\d+)? | [eE]-?\d+)""")
val subject = ((("*"*5)++"1234.567e-6a")*2 ++ " 23E-5 " ++ " 22.0 ")*3

s"SUBJECT: $subject" . show()
"realPat.allPrefixes expecting 12 matches" . show()
realPat.allPrefixes(subject) . show()

"realPat.allSuffixes expecting same 12 matches" . show()
realPat.allSuffixes(subject) . toList . reverse . show()
realPat.reverseCodeListing.show()
realPat.tree.reversed.prettyPrint
realPat.tree.prettyPrint


"realPat prefix and reversed-suffix spans. Expecting 12x2 identical" . show()
realPat.allPrefixes(subject) .
    toList . map ( _.span ) . show()

realPat.allSuffixes(subject) .
    toList . map ( _.span ) . reverse . show()

"*************** WORDS ******************" . show()
val wordPat = Regex("""(\w+)\W""")
val wordsPat= Regex("""(?:\w+\W)+""")

wordsPat.findPrefix(" ====    foobaz is best for you") . show()
wordsPat.findSuffix(" ====    foobaz is best for you") . show()
wordPat.allPrefixes(" ====  xyzzy +++ --- fiddle dedee ") . show()
wordPat.allSuffixes(" ====  xyzzy +++ --- fiddle dedee ") . show()

"URL pattern: reverse scanning (reversed order)" . show()
val urlSubject = "http://www.sputum.com/some/other/url/path/ //ficity.helps span://foo.bar/x/y //wiggle.wogle/foo/ bar"
s"URLSubject: $urlSubject" . show()
val urlPat = Regex("((\\w+):)?(//(?:[.\\w]+/?)+)")
urlPat.reverseCodeListing.show()
urlPat.tree.reversed.prettyPrint.show()
urlPat.allSuffixes(urlSubject) . toList . reverse . show()

"URL pattern: forward scanning (same as reverse scanning)" . show()
urlPat.allPrefixes("http://www.sputum.com/some/other/url/path/ //ficity.helps span://foo.bar //wiggle.wogle/foo bar") . show()



