import sufrin.regex.machine._
import sufrin.regex.syntax
import sufrin.regex.syntax.Tree._
import sufrin.regex.syntax._

val text = "abcd efg abcdefg"

import scala.language.postfixOps
val word   = syntax.Sat((c: Char) => ('a'<=c) && ('z'>=c) , "\\w")
val unWord = syntax.Sat((c: Char) => ! ('a'<=c) || ! ('z'>=c) , "\\W")

var showCode   = true
var traceSteps = true
var tracePos   = true

def trial(label: String, search: Boolean = false, subject: String = text)(pat: Tree[Char]): Unit =
  { val style = if (search) "Search" else "Match"
    if (showCode||traceSteps) println(style)
    val compiled = pat.compile(showCode)
    val state    = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
    val result   = state.run(search, tracePos)
    println(s"$label (${style}) ${pat.source} @ $subject ==> ")
    for { r <- result }
      println(s"==>   $r")
  }

def parse(pat: String): Tree[Char] = new Parser(pat).tree

def trail(label: String, search: Boolean = false, subject: String = text)(pat: String): Unit = {
  val tree = new Parser(pat).tree
  trial(label, search, subject)(tree)
}

//parse("foo")

//parse("(x? | a?b)+bg*?cd")

if (false) {
println("abc")
val p0 = "abc".!
val c0 = p0.compile()
val s = new State[Char](c0, Groups.empty, text, 0, text.length, traceSteps)
s.run(tracePos)
}

/*
trial("")(Alt("abc"!, Span("abcdef"!)))

trial("", true)(Alt("abc"!, Span("abcdef"!)))

trial("", true)(word ++ unWord)

trial("", true)(unWord ++ word)

trial("", true)(word.+ ++ unWord)

trial("", false)(word.+ ++ unWord)

trial("", true)("c".! ++ word.+ ++ unWord)

trial("", true)("c".! ++ word.+ ++ unWord)

trial("", true)("ab".! | "abc".! | "ef".!)

trial("")(Alt(AnchorStart("abcdefx"!), Span("abcdef"!, true)))

trial("")(Alt(AnchorStart("abcdefx"!), Span(AnchorEnd("abcdefg"!), true)))

trial("")("abcde".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("")("abcde-".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("")("a".! | "ab".!  | "abc-".! | "abcde-".!)

trial("", true)("cd".! | "cde".!  | "abc-".! | "abcde-".!)

trial("", true)("cd".! | AnchorEnd("cde".!)  | "abc-".! | "abcde-".!)

trial("", true)("cd".! | AnchorEnd("cdefg".!)  | "abc-".! | "abcde-".!)

trial("", true)("cd".! | ("cdefg".!)  | "abc-".! | "abcde-".!)

trial("", true)  (||("xbc"!, Span("bcd"!), Span("de".!) ++ Span("f".!) ++ ("g".!.?)))

trial("", true)  (||("xbc"!, Span("bcd"!), Span("de".!) ++ Span("f".!) ++ "x".!))

trial("", true) ("ab".! ++ "g".!.? ++ "cd".!)

trial("", true, subject = "fooabggggggcde") ("ab".! ++ "g".!.* ++ "cd".!)

*/

//trial("", true, subject = "fooabggggggcde") (Span("x".!.? | "a".!.?) ++ "b".! ++ "g".!.*? ++ "cd".!)
//trail("", true, subject = "fooabbggggggcde")("(x? | a?)bg*?cd")


//println("****************")
//trial("", true, subject = "fooabbggggggcde") (Span(("x".!.? | "a".!.?) ++ "b".!).+ ++ "b".! ++ "g".!.*? ++ "cd".!)
println("****************")
trail("", true, subject = "fooabbggggggcde")("(x? | a?b)+bg*?cd")



// last match { case Some(Match(ix, all, opta)) => (ix, opta, all) }













