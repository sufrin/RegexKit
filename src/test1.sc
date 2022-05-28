import sufrin.regex.syntax._
import sufrin.regex.machine._

val text = "abcdefg" . toIndexedSeq

def Str(s: String): Regex[Char] = Seq(s.map(Literal(_)))

val p0 = Str("abc")
val c0 = p0.compile()

val s = new State[Char](c0, Groups.empty)
s.run(text, 0, text.length)

val c1 = Alt(Str("ab"), Str("abcd")).compile()
new State(c1, Groups.empty).run(text, 0, text.length)

