package sufrin.regex

object Template {
  import Regex.StringMatch

  trait Template {
    /**
     *  Expand this template, substituting `map(i)` for `Index(i)` and `varMap(n)` for
     * `Variable(n)`
     */
    def subst(map: Int=>String, varMap: String=>String): String

    /**
     * @return the largest `i` for Index(i)` in the template
     */
    def max:  Int

    /**
     * @return the set of `Variable` names in the template.
     */
    def vars: Set[String] = Set.empty
  }

  case class Sequence(templates: Seq[Template]) extends Template {
    def subst(map: Int=>String, varMap: String=>String): String = {
      val out = new StringBuilder
      for {template <- templates} out.append(template.subst(map, varMap))
      out.toString()
    }

    def max: Int = templates.map(_.max).max

    override def vars: Set[String] = templates.map(_.vars).reduce(_.union(_))
  }

  case class Variable(name: String) extends Template {
    def subst(map: Int=>String, varMap: String=>String): String = varMap(name)
    def max: Int = 0
    override def vars: Set[String] = Set(name)
  }

  case class Index(index: Int) extends Template {
    def subst(map: Int=>String, varMap: String=>String): String = map(index)
    def max: Int = index
  }

  case class Text(text: String) extends Template {
    def subst(map: Int=>String, varMap: String=>String): String = text
    def max: Int = 0
  }

  private val rules: List[(String, StringMatch => Template)] = List (
    "[$]([0-9]+)"        -> { case StringMatch(_,digits) => Index(digits.toInt)},
    "[$]([A-Za-z]+)"     -> { case StringMatch(_,name) => Variable(name)},
    "([^$]+)"            -> { case s => Text(s.group(0))},
    "([$][$])"           -> { case s => Text("$")},
  )

  private val lexer   = Regex.fromSources(rules.map(_._1))
  private val mappers = rules.map(_._2)

  def apply(input: CharSequence): Template =  Sequence(lexer.allSymbols(input, mappers).toSeq)
}

