import sufrin.regex.syntax._
import sufrin.regex.machine._

val text = "abcdefg"
def Str(s: String): Regex[Char] = Seq(s.map(Literal(_)))

def trial(pat: Regex[Char], subject: String = text): Option[(Int, Groups)] =
  { // println(s"Trial: $pat")
    val compiled = pat.compile()
    val state = new State[Char](compiled, Groups.empty, subject, 0, subject.length, true)
    state.run(true)
  }

println("abc")
val p0 = Str("abc")
val c0 = p0.compile()
val s = new State[Char](c0, Groups.empty, text, 0, text.length)
s.run()

println("abc|(abcdef)")
trial(Alt(Str("abc"), Group(Str("abcdef"), true)))

println("abcdefx|(abcdef)")
trial(Alt(AnchorStart(Str("abcdefx")), Group(Str("abcdef"), true)))

println("abcdefx|(abcdefg)$")
trial(Alt(AnchorStart(Str("abcdefx")), Group(AnchorEnd(Str("abcdefg")), true)))

