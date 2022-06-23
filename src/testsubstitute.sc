import sufrin.regex.Regex
import sufrin.regex.TestKit._

val pat = Regex("""(\w+)\.(\s+)(\w+)""")

pat.allPrefixes(text) . show()

for { mat <- pat.allPrefixes(text) } println(mat.substitute("$3; $1"))