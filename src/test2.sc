import sufrin.regex.{Match, Regex}

def allStrings (matched: Match[Char]): Seq[String] = {
     (for { (s, e) <- matched.groups.spans } yield
            new String(matched.input.slice(s, e).toArray)).toSeq
}

def show(matched: Match[Char]): Unit = {
    println(allStrings(matched).mkString(", "))
}

val intPat = Regex("""(\d+)""")


intPat.matches("1234567", 0, 7) foreach show

intPat.matches("a1234567a")             foreach show
intPat.findPrefix("a1234567a")          foreach show
intPat.findPrefix(("*"*30)++"1234567a") foreach show

val wordsPat = Regex("""(((\w+)([^\w]|$))+)""",code=true)
val wordsPatW = Regex("""(((\w+)(\W))+)""",code=true)
wordsPat  findPrefix "foobaz is best for you" foreach show
wordsPatW findPrefix "foobaz is best for you" foreach show











