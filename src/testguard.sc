import sufrin.regex._
import TestKit._

val re = Regex("""((a$$$) | b)""", true)
re.allPrefixes("aabaaaba") . show()

val r1 = """((a|b)*)((bc*?)*)((cd|de*)*)(e+)"""
val r2 = """((a|b)*)((bc*)*)((cd|de*)*)(e+)"""

val exp0 = Regex(r1)
val exp1 = Regex(r2)

exp0.allPrefixes("abababccccbccdcdeee") . show()
exp1.allPrefixes("abababccccbccdcdeee") . show()

exp0.allSuffixes("abababccccbccdcdeee") . show()
exp1.allSuffixes("abababccccbccdcdeee") . show()


val exp2 = Regex("""((a|b)+)((bc*)*)((cd|de*)*)(e+)""", true)
exp2.allPrefixes("abababccccbccdcdeee") . show()

