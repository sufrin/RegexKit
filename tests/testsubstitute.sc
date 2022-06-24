import sufrin.regex.Regex
import TestKit._

val pat = Regex("""(\w+)\.(\s+)(\w+)""")

pat.allPrefixes(text) . show()

for { mat <- pat.allPrefixes(text) } println(mat.substitute("$3; $1"))