import sufrin.regex.syntax._
import sufrin.regex.machine._

val text = "abcdefg"
def Str(s: String): Regex[Char] = Seq(s.map(Literal(_)))

val showCode   = false
val traceSteps = true
val tracePos   = true

def trial(label: String, search: Boolean = false)(pat: Regex[Char], subject: String = text): Unit =
  {
    val compiled = pat.compile(showCode)
    val state    = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
    val result   = state.run(search, tracePos)
    println()
    println(s"Trial: $label = $result")

  }

if (false) {
println("abc")
val p0 = Str("abc")
val c0 = p0.compile()
val s = new State[Char](c0, Groups.empty, text, 0, text.length, traceSteps)
s.run(tracePos)
}

trial("abc|(abcdef)")(Alt(Str("abc"), Group(Str("abcdef"), true)))

trial("abcdefx|(abcdef)")(Alt(AnchorStart(Str("abcdefx")), Group(Str("abcdef"), true)))

trial("abcdefx|(abcdefg)$")(Alt(AnchorStart(Str("abcdefx")), Group(AnchorEnd(Str("abcdefg")), true)))

trial("abcde|a|ab|abc|abcd")(Str("abcde") | Str("a") | Str("ab")  | Str("abc") | Str("abcd"))

trial("abcde-|a|ab|abc|abcd")(Str("abcde-") | Str("a") | Str("ab")  | Str("abc") | Str("abcd"))

trial("a|ab|abc-|abcde-")(Str("a") | Str("ab")  | Str("abc-") | Str("abcde-"))

trial("cd|cde|abc-|abcde-", true)(Str("cd") | Str("cde")  | Str("abc-") | Str("abcde-"))

trial("cd|cde$|abc-|abcde-", true)(Str("cd") | AnchorEnd(Str("cde"))  | Str("abc-") | Str("abcde-"))

trial("cd|cdefg$|abc-|abcde-", true)(Str("cd") | AnchorEnd(Str("cdefg"))  | Str("abc-") | Str("abcde-"))

trial("cd|cdefg|abc-|abcde-", true)(Str("cd") | (Str("cdefg"))  | Str("abc-") | Str("abcde-"))
