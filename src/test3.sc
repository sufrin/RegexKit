/**
 * This demonstrates the not-so subtle difference between
 * suffix-matching of patterns with/without starred
 * expressions.
 */

import sufrin.regex.Regex
import sufrin.regex.Regex.StringMatch

val realPatPlus = Regex("""\d+(?: \.\d+  [eE]-?\d+ | [eE]-?\d+)""")

realPatPlus.findSuffix("1234.567e6aaaa") .
  map {
    case StringMatch(s) => s
  } . toList


val realPatStar = Regex("""\d+(?: \.\d*  [eE]-?\d+ | [eE]-?\d+)""", showCode = true)

realPatStar.findSuffix("1234.567e6aaaa") .
  map {
    case StringMatch(s) => s
  } . toList


realPatStar.findSuffix("1234.567aaaa") .
  map {
    case StringMatch(s) => s
  } . toList

realPatStar.findPrefix("aaa1234.567e6aaaa") .
  map {
    case StringMatch(s) => s
  } . toList

val revPatStar = Regex("""(?:[\d]+-?[eE][\d]*[\.] | [\d]+-?[eE])[\d]+""", true, true)

revPatStar.tree == realPatStar.tree.reversed

revPatStar.prefixes("1234.567e6".reverse)

val revPatPlus = Regex("""(?:[\d]+-?[eE][\d]+[\.] | [\d]+-?[eE])[\d]+""", true, true)

revPatPlus.tree == realPatStar.tree.reversed

revPatPlus.prefixes("1234.567e6".reverse)

