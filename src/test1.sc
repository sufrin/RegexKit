import sufrin.regex.TestKit._

//ends("-cr")("abc|(abcdef)")
//ends("-cr")("(abc)+|(abcdef)")
starts("c", "abcdefg")("(abcx)|(ab(cd)(ef))(g)")

starts("c", "1")("(\\d+)")

starts("c", "12233344445555")("(\\d+)")

starts("c", "12233344445555")("((\\d)+)")

starts("cs@", "122 33344445555")("((\\d)+)")

find("cs@", "abc 1 22 33344445555")("(\\d+)")

find("cs@", "abc 1111111111 22 33344445555")("(\\d+)(\\D|$)")

all("c", "abc 1 22 333 4444 55555")("(\\d+)(\\D|$)")

all("c", "abc 1 22 333 4444 55555")("(\\d+)(\\D|$)")

all("c", "abc 5555555 1 22 333 4444 55555 ")("(\\d+)")

all("c", "abc 1 22 333 4444 55555 ")("(5+)[^5]")

find("s@c", "abc55555")("(5+)")

all("s@c", "abc 1 22 5555555555 333 4444 55555")("(5+)")




starts("c", "12233344445555")("(\\d+)(5)")
starts("c", "12233344445555")("(\\d+)(55)")
starts("c", "12233344445555")("((\\d)+)(55)")




/* Repetition of a nilpotent RE
//find("@cs", "fooabbggggggcde")("(x? | a?b)+bg*?cd")
//pprint(parse("(x? | a?b)+bg*?cd"))
(parse("(x? | a?b)+bg*?cd")).compile(true)
*/
// PROBLEMATIC
//find("@cs", "fooabbggggggcde")("(a?b)+bg*?cd")
//find("@cs", "fooabbggggggcde")("([^ab])+?(a?b| x?)+bg*?cd")
//find("@cs", "fooabbggggggcde")("([^ab])*(a?b| x?)+bg*?cd")


if (false) {
  //def span(t: Tree[Char]): Tree[Char] = Span(false, false, t)
  //run("", "run")(Alt("abc"!, span("abcdef"!)))
  starts("")("abc|(abcdef)")
  println("============")
  find("")("abc|(abcdef)")
  println("============")
  all("")("abc|(abcdef)")
  println("============")
  all("")("efg|(abcdef)")
  println("============")
  all("", "the bcd representxy is foxed")("bcd|(efg$)|xy|(abcdef)")
  println("============")
  all("@cs", "ten is  10 andnext is 123")("(\\d+)(\\D|$)")
  println("============")
  all("@cs", "ten is 10 next is 123")("\\D(\\d+)")
  println("============")
  all("@cs", "one 1 two 10 three 133  four 4444 three 123 ")("(\\d+)")
  println("============")

  all("@c", "one 1 two 22 three 333  four 4444 three 123 ")("(\\d+)(\\D)")
  all("@cs", "one 1 two 22 three 333  four 4444 three 123 ")("(\\d+)")
  find("@cs", "one 1 two 22 three 333  four 4444 three 123 ")("((\\d+)\\D+?)+")
}

