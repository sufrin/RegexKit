/**
 * This demonstrates the not-so subtle difference between
 * suffix-matching of patterns with/without starred
 * expressions.
 */

import sufrin.regex._
import TestKit._

val exp0 = Regex("""((a|b)*)((bc*?)*)((cd|de*)*)(e*)""")
val exp1 = Regex("""((a|b)*)((bc*)*)((cd|de*)*)(e*)""")
exp0.matches("abababccccbccdcdeee") . show()
exp1.matches("abababccccbccdcdeee") . show()

val exp3 = Regex("""(abc|abcd).*""")
val exp3a = Regex("""(abc|abcd)""")
val exp4 = Regex("""(abcd|abc).*""")
val exp4a = Regex("""(abcd|abc)""")
"Prefixes: exp3, exp3a, exp4, exp4a".show()
exp3.allPrefixes("xxxabcdefg and abcdef") . show()
exp3a.allPrefixes("xxxabcdefg and abcdef") . show()
exp4.allPrefixes("xxxabcdefg and abcdef") . show()
exp4a.allPrefixes("xxxabcdefg and abcdef") . show()
"Suffixes: exp3, exp3a, exp4, exp4a". show()
exp3.allSuffixes("xxxabcdefg and abcdef") . show()
exp3a.allSuffixes("xxxabcdefg and abcdef") . show()
exp4.allSuffixes("xxxabcdefg and abcdef") . show()
exp4a.allSuffixes("xxxabcdefg and abcdef") . show()

val numPat = Regex("""\d+(( \.\d+[eE]-?\d+ | [eE]-?\d+ | \.\d+ )??)""")
numPat.matches("1234.567e6") . show()
numPat.findPrefix("aaaaa1234.567e6") . show()
numPat.matches("1234.567") . show()
numPat.findPrefix("aaaaa1234.567") . show()

val realPatPlus  = Regex("""\d+(?: (?: \.\d+  [eE]-?\d+ ) | (?: \.\d+))""")
val realPatStar  = Regex("""\d+(?: (?: \.\d*  [eE]-?\d+ ) | (?: \.\d+))""")
val realPatStarR = Regex("""\d+(?: (?: \.\d+) | (?: \.\d*  [eE]-?\d+ ) )""")

realPatPlus.findSuffix("1234.567e-60aaaa") . show()
realPatStar.findSuffix("1234.567e-60aaaa") . show()
realPatStar.findSuffix("1234.567e-60aaaa") . show()

"""Demonstrates priority of alt on left""".show()
realPatStar.findPrefix("aaa1234.567e-60aaaa") . show()
realPatStarR.findPrefix("aaa1234.567e-60aaaa") . show()

realPatPlus.findPrefix("aaa1234.567e-60aaaa") . show()
realPatStar.findPrefix("aaa1234.e-60aaaa") . show()
realPatPlus.findPrefix("aaa1234.e-60aaaa") . show()
realPatStar.findSuffix("aaa1234.e-60aaaa") . show()
realPatPlus.findSuffix("aaa1234.e-60aaaa") . show()

val revPatStar = Regex(realPatStar.tree.reversed, false, false)
val revPatPlus = Regex(realPatStar.tree.reversed, false, false)
// revPatStar.tree == realPatStar.tree.reversed
revPatStar.prefixes("1234.567e6".reverse) . show()
//revPatPlus.tree == realPatStar.tree.reversed
revPatPlus.prefixes("1234.567e6".reverse) . show()
