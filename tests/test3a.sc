import sufrin.regex._
import TestKit._

val nlexp  = Regex("""([^\n]*)(?:\n|$)""")
val nlexpa = Regex("([^\n]*)(?:\n|$)")
nlexp.allPrefixes("a\nab\nxy").show()
nlexpa.allPrefixes("a\nab\nxy").show()
nlexp.allSuffixes("a\nab\nxy").revShow()
nlexpa.allSuffixes("a\nab\nxy").revShow()


