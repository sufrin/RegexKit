/**
 * Testing of the basic tools underlying the API
 * This shows that the `searching` mode
 * of the abstract machine finds the end position of a match
 * accurately, but does not generate correct captures
 * en-route; and (a fortiori) gets the starting
 * position of the match wrong.
 *
 * This is why we stick to the (potentially quadratic
 * in the length of the subject) method of sliding
 * a potential prefix along the subject to perform a search.
 * BS: 10 June 2021
 */

import sufrin.regex.TestKit._

search("", "fooabbggggggcde")("(a?b)+bg*?cd")
"starts with repeats, unprefixed and unsuffixed". show()
starts("", "abcdefg 1111111111 22 3334444 111 5555")("\\w+")
"".show()
starts("", "1111111111 22 33344445555")("(\\d+)")
"starts finds the longest".show()
starts("", "1111111111 22 33344445555")("(\\d+\\D)+")
"search finds the shortest".show()
search("", "abcd1111111111 22 33344445555")("(?:(\\d+)\\D)+")
search("", "abcd1111111111 22 33344445555")("[^\\d]+(?:(\\d+)\\D)+")
search("", "abcd1111111111 22 33344445555")("[^\\d]*(?:(\\d+)\\D)+")
starts("", "abcd1111111111 22 33344445555")("[^\\d]+(?:(\\d+)\\D)+")

"search with a leading \\D* is nonterminating -- suppressed here".show()
// search("", "abcd1111111111 22 33344445555")("\\D*(\\d+\\D)+")

"SEARCHES with repeats: failure means the spans are too short, though they may end in the right place ".show()
"trailing \\D succeeds".show()
search("", "abcdefg 1111111111 22 333444455555")("""\D(\d+)\D""")
"lack of trailing \\D fails".show()
search("", "abcdefg 1111111111 22 333444455555")("""\D(\d+)""")
"lack of trailing \\D in quadratic method".show()
find("", "abcdefg 1111111111 22 333444455555")("""\D(\d+)""")
"trailing 4? fails".show()
search("", "abcdefg 1111111111 22 333444455555")("""(\d+4?)""")
"trailing 4 succeeds".show()
search("", "abcdefg 1111111111 22 333444455555")("""(\d+4)""")
"trailing 4? ".show()
starts("", "abcdefg1111111111 22 333444455555")("""\w+((\d|\s)+(4?))""")
"trailing 4?? shouldn't be the same".show()
starts("", "abcdefg1111111111 22 333444455555")("""\w+((\d|\s)+(5+?))""")
"trailing ??4 shouldn't be the same".show()
search("", "abcdefg 1111111111 22 333444455555")("""\D(\d+??4)""")
"trailing $ works".show()
search("", "abcdefg 1111111111 22 333444455555")("""\D(\d+$)""")

"SEARCHES with literals: failure means the spans are too long! ".show()
"Succeeds at a junction".show()
search("", "abc 2222 3334444 111 5555")("34")
"Succeeds for pattern size >=4 ".show()
search("", "abc 2222 3334444 111 5555")("2222")
search("", "abc 2222 3334444 111 5555")("3444")
"Fails for pattern size 2,3 in repetitive subject; with too long a match ".show()
search("cs", "abc 2222 3334444 111 5555")("44")
search("cs", "abc 2222 3334444 111 5555")("444")
"Quadratic method in repetitive subject ".show()
find("", "abc 2222 3334444 111 5555")("44")
findAll("", "abc 2222 3334444 111 5555")("44")
find("", "abc 2222 3334444 111 5555")("444")
find("", "abc 2222 3334444 111 5555")("4444")
"Succeeds in non-repetitive subject".show()
search("", "abc 2222 3334445 111 5555")("2")
search("", "abc 2222 3334445 111 5555")("5")
search("", "abc 2222 3334445 111 5555")("4")
search("cs", "abc 2222 3334445 111 5555")("44")
search("cs", "abc 2222 3334445 111 5555")("444")


"Suffix succeeds".show()
search("", "abc 2222 3334444 111 5555")("(22)[^2]")
"Prefix + suffix succeeds".show()
search("", "abc 2222 3334444 111 5555")("([^2])(22)([^2]*)")
"Prefix, no suffix: succeeds".show()
search("", "abc 2222 3334444 111 5555")("[^2](22)")

"All instances with \\D after \\d+ generates appropriate matches".show()
all("", "abc 2222 3334444 111 5555")("""(?:(\d+)\D)""")
"All instances without \\D after \\d+ generates spurious partial matches".show()
all("", "abc 2222 3334444 111 5555")("""(?:(\d+))""")




if (false) {
  "".show()
  starts("c", "1")("(\\d+)")
  "".show()
  starts("c", "122333444455555")("(\\d+)")
  "".show()
  starts("c", "122333444455555 ")("(\\d+)\\D")
  "".show()
  starts("c", "122333444455555")("((\\d)+)")
  "".show()
  starts("c", "122 33344445555")("((\\d)+)")
  "".show()
  starts("cs@", "1111111111 22 33344445555")("(\\d+)")
  "".show()
  starts("cs@", "1111111111 22 33344445555")("\\d\\d*")
  "".show()
  search("cs@", "abcdefg 1111111111 22 33344445555")("\\D(\\d+)\\D")
  "".show()
  search("cs@", "abcdefg 1111111111 22 33344445555")("1+")
  "".show()
  search("cs@", "abcdefg 1111111111 22 33344445555")(".*?\\d+(\\D|$)")
  "".show()
}

if (false) {
  "".show()
  starts("", "abcdefg 1111111111 22 3334444 111 5555 ")("(\\d+)(\\D|$)")
  "".show()
  starts("c", "12233344445555")("(\\d+)(5)")
  starts("c", "12233344445555")("(\\d+)(55)")
  starts("c", "12233344445555")("((\\d)+)(55)")

}



/* Repetition of a nilpotent RE
//search("@cs", "fooabbggggggcde")("(x? | a?b)+bg*?cd")
//prettyPrint(parse("(x? | a?b)+bg*?cd"))
(parse("(x? | a?b)+bg*?cd")).compile(true)
*/
// PROBLEMATIC
//search("@cs", "fooabbggggggcde")("(a?b)+bg*?cd")
//search("@cs", "fooabbggggggcde")("([^ab])+?(a?b| x?)+bg*?cd")
//search("@cs", "fooabbggggggcde")("([^ab])*(a?b| x?)+bg*?cd")


if (false) {
  //def span(t: Tree[Char]): Tree[Char] = Span(false, false, t)
  //experiment("", "experiment")(Alt("abc"!, span("abcdef"!)))
  val subj = "abcde abcdef abcdefg"
  starts("", subj)("abc|(abcdef)")
  "".show()
  search("", subj)("abc|(abcdef)")
}

