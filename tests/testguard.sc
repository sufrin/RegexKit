import sufrin.regex._
import TestKit._

val pattern = """(\w\w*)(((\.)\w\w*)?)"""
val subject = "abcde.fghij pqrs == "
s"Guarded compilation of \"$pattern\"".show()
Regex.guarding=true
val r0 = Regex(pattern)
s"Prefixes: $subject".show()
r0.prefixes("abcde.fghij pqrs ==").show()
r0.allPrefixes("===abcde.fghij pqrs ==").show()
r0.forwardInstructions.show()
r0.tree.prettyPrint.show()

s"Suffixes: $subject".show()
r0.allSuffixes("===abcde.fghij pqrs ==").show()
r0.reverseInstructions.show()
r0.tree.prettyPrint.show()

s"Unguarded compilation of \"$pattern\"".show()
Regex.guarding=false
val r1 = Regex(pattern)
s"Prefixes: $subject".show()
r1.prefixes("abcde.fghij pqrs ==").show()
r1.allPrefixes("===abcde.fghij pqrs ==").show()
r0.forwardInstructions.show()
r0.tree.prettyPrint.show()

s"Suffixes: $subject".show()
r1.allSuffixes("===abcde.fghij pqrs ==").show()
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
