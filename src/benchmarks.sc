/**
 *  Benchmarking performance of some of the
 *  search methods. In particular those thought to
 *  take time quadratic in the length of the subject
 *  strings when they fail.
 */


import sufrin.regex.{Regex, TestKit}
import sufrin.regex.TestKit._

"************* Benchmarks".show()
object Sample {
  val len   = TestKit.text.length
  val texts = List (text*16, text*8, text*4, text*2, text)
}
val wordPat = Regex("""\W(\w+)\W""")
val failPat = Regex("""(\w+)\.\.""")
val succPat = wordPat
val linePat = Regex ("""([^\n]+)\R""")
val nlPat   = Regex("\\n")

s"************* Benchmarks for successful searching ($succPat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(wordPat.allPrefixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}

s"************* Benchmarks for successful backward searching ($succPat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(wordPat.allSuffixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}

s"************* Benchmarks for failing searching ($failPat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(failPat.allPrefixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}

s"************* Benchmarks for failing backward searching ($failPat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(failPat.allSuffixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}

"******** Now searching for lines".show()

s"************* Benchmarks for successful searching ($linePat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(linePat.allPrefixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}

s"************* Benchmarks for successful searching ($nlPat)".show()
for { text <- Sample.texts } {
  val (r, t) = timing(nlPat.allPrefixes(text).toList.size)
  f"${text.length}%10d chars $r%5d matches ${t.toDouble/10E9}%10e" . show()
}


























