import sufrin.regex.syntax
import sufrin.regex.syntax._
import sufrin.regex.machine._
import sufrin.regex.{Match, machine}
import Tree._

val text = "abcd efg abcdefg"
import language.postfixOps
val word = syntax.Sat((c: Char) => ('a'<=c) && ('z'>=c) , "\\w")
val unWord = syntax.Sat((c: Char) => ! ('a'<=c) || ! ('z'>=c) , "\\W")

val showCode   = false
val traceSteps = false
val tracePos   = false
var last: Option[Match[Char]] = null

def trial(label: String, search: Boolean = false, subject: String = text)(pat: Tree[Char]): Unit =
  {
    val compiled = pat.compile(showCode)
    val state    = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
    val result   = state.run(search, tracePos)
    for { r <- result }
      println(s"$label ${pat.source} @ $subject ==> $r")
  }

if (false) {
println("abc")
val p0 = "abc".!
val c0 = p0.compile()
val s = new State[Char](c0, Groups.empty, text, 0, text.length, traceSteps)
s.run(tracePos)
}

trial("")(Alt("abc"!, Span("abcdef"!, true)))

trial(label="Multi", true)(word ++ unWord)

trial(label="Multi", true)(unWord ++ word)

trial(label="Multi", true)(word.+ ++ unWord)

trial(label="Multi", true)("ab".! | "abc".! | "ef".!)

trial("")(Alt(AnchorStart("abcdefx"!), Span("abcdef"!, true)))

trial("")(Alt(AnchorStart("abcdefx"!), Span(AnchorEnd("abcdefg"!), true)))

trial("")("abcde".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("")("abcde-".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("")("a".! | "ab".!  | "abc-".! | "abcde-".!)

trial("Search", true)("cd".! | "cde".!  | "abc-".! | "abcde-".!)

trial("Search", true)("cd".! | AnchorEnd("cde".!)  | "abc-".! | "abcde-".!)

trial("Search", true)("cd".! | AnchorEnd("cdefg".!)  | "abc-".! | "abcde-".!)

trial("Search", true)("cd".! | ("cdefg".!)  | "abc-".! | "abcde-".!)

trial("Search", true)  (||("xbc"!, Span("bcd"!), Span("de".!) ++ Span("f".!) ++ ("g".!.?)))

trial("Search", true)  (||("xbc"!, Span("bcd"!), Span("de".!) ++ Span("f".!) ++ "x".!))

trial("Search", true) ("ab".! ++ "g".!.? ++ "cd".!)

trial("Search", true, subject = "fooabggggggcde") ("ab".! ++ "g".!.* ++ "cd".!)

trial("Search", true, subject = "fooabggggggcde") (Span("x".!.? | "a".!.?) ++ "b".! ++ "g".!.*? ++ "cd".!)


// last match { case Some(Match(ix, all, opta)) => (ix, opta, all) }













