/**
 * This demonstrates the not-so subtle difference between
 * suffix-matching of patterns with/without starred
 * expressions.
 */

import sufrin.regex._
import TestKit._

def testCan(pat: String, s: String): Unit = {
  val t = Regex(pat).tree
  for { c <- s } if (t.canStartWith(c))
    print(s"$c ")
  println()
  t.prettyPrint
}

val exp0 = Regex("""((a|b)*)((bc*?)*)((cd|de*)*)(e*)""")
val exp1 = Regex("""((a|b)*)((bc*)*)((cd|de*)*)(e*)""")
exp0.matches("abababccccbccdcdeee") . show()
exp1.matches("abababccccbccdcdeee") . show()

testCan("""(a?b?cd | xyz)""", "abcdefgpqrstuvxyz12345")
testCan("""(xyz)""", "abcdefgpqrstuvxyz12345")





val numPat = Regex("""\d+(( \.\d+[eE]-?\d+ | [eE]-?\d+ | \.\d+ )??)""")
numPat.matches("1234.567e6") . show()
numPat.findPrefix("aaaaa1234.567e6") . show()
numPat.matches("1234.567") . show()
numPat.findPrefix("aaaaa1234.567") . show()

val realPatPlus = Regex("""\d+(?: (?: \.\d+  [eE]-?\d+ ) | (?: \.\d+))""")
val realPatStar = Regex("""\d+(?: (?: \.\d*  [eE]-?\d+ ) | (?: \.\d+))""")

realPatPlus.findSuffix("1234.567e6aaaa") . show()
realPatStar.findSuffix("1234.567e6aaaa") . show()
realPatStar.findSuffix("1234.567e-60aaaa") . show()
realPatStar.findPrefix("aaa1234.567e-6aaaa") . show()
realPatPlus.findPrefix("aaa1234.567e-6aaaa") . show()
realPatStar.findPrefix("aaa1234.e-6aaaa") . show()
realPatPlus.findPrefix("aaa1234.e-6aaaa") . show()
realPatStar.findSuffix("aaa1234.e-6aaaa") . show()
realPatPlus.findSuffix("aaa1234.e-6aaaa") . show()

val revPatStar = Regex(realPatStar.tree.reversed, false, false)
val revPatPlus = Regex(realPatStar.tree.reversed, false, false)
// revPatStar.tree == realPatStar.tree.reversed
revPatStar.prefixes("1234.567e6".reverse) . show()
//revPatPlus.tree == realPatStar.tree.reversed
revPatPlus.prefixes("1234.567e6".reverse) . show()
