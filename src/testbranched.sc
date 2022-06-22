import sufrin.regex.TestKit._
import sufrin.regex.Brackets

val text = "foo(bar is ((best on thursday) for you) on ) thursday"

findAllBranches("c", text)("\\((\\w+)", "(\\))", "(f?)(\\w+)")

val paren = Brackets("""\(""", """\)""")

//print(paren.bracket.forwardInstructions)
//print(paren.bracket.reverseInstructions)
//paren.bracket.findSuffix(text, 0, text.length)
//paren.bracket.allPrefixes(text, 0, text.length) . show()
//paren.bracket.allSuffixes(text, 0, text.length) . show()

paren.matchForward(text, 9, text.length)
paren.matchForward(text, 12, text.length)
paren.matchForward(text, 11, text.length)
paren.matchBackward(text, 0, text.length)