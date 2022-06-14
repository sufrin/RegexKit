import sufrin.regex._
import TestKit._

def testCan(pat: String, s: String): Unit = {
  val t = Regex(pat).tree
  for { c <- s } if (t.canStartWith(c))
    print(s"$c ")
  println()
  t.prettyPrint
}

val r1 = """((a|b)*)((bc*?)*)((cd|de*)*)(e+)"""
val r2 = """((a|b)*)((bc*)*)((cd|de*)*)(e+)"""
testCan(r1,"abcdefgpqrstuvxyz12345")

val exp0 = Regex(r1)
val exp1 = Regex(r2)

exp0.allPrefixes("abababccccbccdcdeee") . show()
exp1.allPrefixes("abababccccbccdcdeee") . show()

exp0.allSuffixes("abababccccbccdcdeee") . show()
exp1.allSuffixes("abababccccbccdcdeee") . show()


val exp2 = Regex("""((a|b)+)((bc*)*)((cd|de*)*)(e+)""", true)
exp2.allPrefixes("abababccccbccdcdeee") . show()

testCan("""(a?b?cd | xyz)""", "abcdefgpqrstuvxyz12345")
testCan("""(xyz)""", "abcdefgpqrstuvxyz12345")
