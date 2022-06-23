import sufrin.regex.TestKit._
import sufrin.regex.Brackets

val text = "foo(bar is ((best on thursday) for you) on ) thursday"

"Testing branch-matching and grouping".show()
findAllBranches("n", text)("\\((\\w+)", "(\\))", "(f?)(\\w+)")
findAllBranches("n", text)("""(f\w+)""", """(\W)(o\w+)""", """((?:b|t)\w+)""")

"Testing bracket-matching".show()
val paren = Brackets("""\(""", """\)""")
paren.matchForward(text, 9, text.length)
paren.matchForward(text, 12, text.length)
paren.matchForward(text, 11, text.length)
paren.matchBackward(text, 0, text.length)

//print(paren.bracket.forwardInstructions)
//print(paren.bracket.reverseInstructions)
//paren.bracket.findSuffix(text, 0, text.length)
//paren.bracket.allPrefixes(text, 0, text.length) . show()
//paren.bracket.allSuffixes(text, 0, text.length) . show()
