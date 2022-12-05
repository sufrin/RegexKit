package sufrin.regex

object Rewrite {
  /**
   * A string rewriter specified by a sequence of rules of the form
   * {{{pattern -> template}}}
   * where `pattern` is a regular expression, and `template` is the template used
   * to rewrite fragments of text that match the pattern. When
   * a template is used to rewrite such a fragment it is
   * interpreted literally, except that:
   *
   * `$`*n* (where *n* is an integer) is rewritten as the *n*th parenthesised group
   * of the matched pattern.
   *
   * @param rules the sequence of `(pattern -> template)` rewrite rules.
   */
  class Rules(rules: Seq[(String, String)]) {
    override def toString: String = {
      rules.mkString("Rules(", "->", ")")
    }
    private val patterns  = rules.map(_._1)
    private val templates = rules.map(_._2).map(Template(_))
    private val scan      = Regex.fromSources(patterns)

    def apply(input: CharSequence): String =
      { val matches = scan.allPrefixes(input)
        val out = new StringBuilder
        while (matches.hasNext) {
          val m = matches.next
          val t = templates(m.theMatch.index)
          out.append(t.subst({case n: Int => m.group(n)} , { case name: String => "$"+name}))
        }
        out.toString()
      }
  }
}
