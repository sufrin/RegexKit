var trace = false

def tr(text: String): Unit = {
  val t = new sufrin.regex.syntax.Parser(text, trace)
  println(s"---: $text")
  sufrin.regex.Util.pprint(t.tree)
  println(t.tree.source)
}


tr("abc")

tr("abc|def")

tr("abc|de?f*")

tr("((abc)*|de?f*)?")

tr("((ab(c*)*)*|de?f*)?")

tr("(x? | a?b)+bg*?cd")




