import TestKit._
import sufrin.regex.Brackets
import sufrin.regex.Regex.StringMatch

val text = "foo(bar is ((best on THURSDAY) for you) on ) thursday"

"Testing the rewriteAll API".show()
val lex = sufrin.regex.Regex.fromSources(List("[(]", "[)]", "([0-9]+)", "([a-z]+)", "([A-Z]+)"))
lex.rewriteAll(text, List("{BR}", "{KT}", "{NUM($1)}", "{LC($1)}", "{UC($1)}"), false).show()

"Testing the allSymbols API".show()
val kind: StringMatch=>String = {
  case s: StringMatch => s"${s.theMatch.index}: ${s.group(1, "-")}"
}

lex.allSymbols(text, List(kind, kind, kind, kind, kind)).show()

"Testing branch-matching and grouping".show()
findAllBranches("n", text)("\\((\\w+)", "(\\))", "(f?)(\\w+)")
findAllBranches("n", text)("""(f\w+)""", """(\W)(o\w+)""", """((?:b|t)\w+)""")

"Testing bracket-matching".show()
val paren = Brackets("""\(""", """\)""")
paren.matchForward(text, 9, text.length)
paren.matchForward(text, 12, text.length)
paren.matchForward(text, 11, text.length)
paren.matchBackward(text, 0, text.length)
paren.matchForward(text, 3, text.length)

//print(paren.bracket.forwardCodeListing)
//print(paren.bracket.reverseCodeListing)
//paren.bracket.findSuffix(text, 0, text.length)
//paren.bracket.allPrefixes(text, 0, text.length) . show()
//paren.bracket.allSuffixes(text, 0, text.length) . show()

findAllBranches("n", programtext)(
  """(\w+)""",
  """\(""",
  """\)""",
  """\{""",
  """\}""",
  """<-""",
  """==""",
  """=""",
  """,""",
  """\.""",
  """;""",
  """("([^"\\]*(\\.))*")""", // delicate
  """/\*.*\*/"""             // non-nesting
)