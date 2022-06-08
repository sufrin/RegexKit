import sufrin.regex.Regex

val intPat = Regex("""(\d+)""")

intPat.matches("1234567")
intPat.matches("a1234567a")
intPat.findPrefix("a1234567a")
intPat.findPrefix(("*"*30)++"1234567a")

val wordsPat = Regex("""(((\w+)([^\w]|$))+)""")
val wordsPatW = Regex("""(((\w+)(\W))+)""", true, true)

wordsPat  findPrefix "foobaz is best for you"
wordsPatW findPrefix "foobaz is best for you"










