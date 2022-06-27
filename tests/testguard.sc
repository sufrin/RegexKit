import sufrin.regex._
import TestKit._

val pattern = """(\w+)((=\w+)?)"""
val subject = "abcde=fghij pqrs == "
s"Guarded compilation of \"$pattern\"".show()
Regex.guarding=true
val r0 = Regex(pattern)
s"Prefixes: (MATCH STOPS TOO EARLY) $subject".show()
// Works fine when the pattern is `(\w+)((=\w+)+)`
// This points to a problem with guarded R? compilation.
r0.prefixes(subject).show()
r0.matches("abcde=fghij").show()
r0.allPrefixes(subject).show()
r0.forwardInstructions.show()
r0.tree.prettyPrint.show()

s"Suffixes: (CORRECT) $subject".show()
r0.allSuffixes(subject).show()
r0.reverseInstructions.show()
r0.tree.prettyPrint.show()

s"Unguarded compilation of \"$pattern\"".show()
Regex.guarding=false
val r1 = Regex(pattern)
s"Prefixes: (CORRECT) $subject".show()
r1.prefixes(subject).show()
r1.matches("abcde=fghij").show()
r1.allPrefixes(subject).show()
r1.forwardInstructions.show()
r1.tree.prettyPrint.show()

s"Suffixes: (CORRECT) $subject".show()
r1.allSuffixes(subject).show()
r0.reverseInstructions.show()
r0.tree.prettyPrint.show()


if (false) {
  val re = Regex("""((a$$$) | b)""", true)
  re.allPrefixes("aabaaaba").show()

  val r1 = """((a|b)*)((bc*?)*)((cd|de*)*)(e+)"""
  val r2 = """((a|b)*)((bc*)*)((cd|de*)*)(e+)"""

  val exp0 = Regex(r1)
  exp0.tree.prettyPrint.show()

  val exp1 = Regex(r2)
  exp1.tree.prettyPrint.show()

  exp0.allPrefixes("abababccccbccdcdeee").show()
  exp1.allPrefixes("abababccccbccdcdeee").show()

  exp0.allSuffixes("abababccccbccdcdeee").show()
  exp1.allSuffixes("abababccccbccdcdeee").show()


  val exp2 = Regex("""((a|b)+)((bc*)*)((cd|de*)*)(e+)""", true)
  exp2.allPrefixes("abababccccbccdcdeee").show()
}
