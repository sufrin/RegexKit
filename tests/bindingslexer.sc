import sufrin.regex.Regex
import sufrin.regex.syntax.{Parser, Branched}

/** A mini lexer  */

def symbols(pats: String*): Regex =
{
  val trees = pats.map(pat => new Parser(pat).tree)
  new Regex(Branched(trees), false, false)
}

val scanner = symbols("""\s*([a-zA-Z.\-]+)""", """\s*(\\x\w\w\w\w)""", """\s*\"(?:\\x\w\w\w\w|[^"])+\"""")

val matches = scanner.allPrefixes("""the "rain" in "Spai\x1234n" is \x002E """).toList

for { item <- matches } println(item.theMatch.index, item.matched)
