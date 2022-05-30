import sufrin.regex.syntax._
import Tree._
import sufrin.regex.machine._

val text = "abcdefg"
import language.postfixOps


val showCode   = true
val traceSteps = true
val tracePos   = true

def trial(label: String, search: Boolean = false, subject: String = text)(pat: Tree[Char]): Unit =
  {
    val compiled = pat.compile(showCode)
    val state    = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
    val result   = state.run(search, tracePos)
    println()
    println(s"Trial: $label = $result")

  }

if (false) {
println("abc")
val p0 = "abc"!
val c0 = p0.compile()
val s = new State[Char](c0, Groups.empty, text, 0, text.length, traceSteps)
s.run(tracePos)
}

trial("abc|(abcdef)")(Alt("abc"!, Group("abcdef"!, true)))

trial("abcdefx|(abcdef)")(Alt(AnchorStart("abcdefx"!), Group("abcdef"!, true)))

trial("abcdefx|(abcdefg)$")(Alt(AnchorStart("abcdefx"!), Group(AnchorEnd("abcdefg"!), true)))

trial("abcde|a|ab|abc|abcd")("abcde".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("abcde-|a|ab|abc|abcd")("abcde-".! | "a".! | "ab".!  | "abc".! | "abcd".!)

trial("a|ab|abc-|abcde-")("a".! | "ab".!  | "abc-".! | "abcde-".!)

trial("cd|cde|abc-|abcde-", true)("cd".! | "cde".!  | "abc-".! | "abcde-".!)

trial("cd|cde$|abc-|abcde-", true)("cd".! | AnchorEnd("cde".!)  | "abc-".! | "abcde-".!)

trial("cd|cdefg$|abc-|abcde-", true)("cd".! | AnchorEnd("cdefg".!)  | "abc-".! | "abcde-".!)

trial("cd|cdefg|abc-|abcde-", true)("cd".! | ("cdefg".!)  | "abc-".! | "abcde-".!)

trial("||(abc,bcdef)", true)  (||("xbc"!, "foo"!, Group("def"!)+Group("g"!)))

trial("abg?cd", true) ("ab".! + "g".!.? + "cd".!)

trial("abg?cd", true, subject = "fooabggggggcde") ("ab".! + "g".!.* + "cd".!)









