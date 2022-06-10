package sufrin.regex

object TestKit {
  import sufrin.regex.machine._
  import sufrin.regex.syntax._
  import scala.language.postfixOps

  val word   = syntax.Sat((c: Char) => ('a'<=c) && ('z'>=c) , "\\w")
  val unWord = syntax.Sat((c: Char) => ! ('a'<=c) || ! ('z'>=c) , "\\W")

  implicit class Title(s: String) {
    def *** : Unit = println(s"======== $s")
  }

  var result: Option[Match[Char]] = None

  def experiment(trace: String = "", label: String, search: Boolean = false, subject: String, start: Int = 0)(pat: Tree[Char]): Unit = {
    var showCode      = false
    var showTree      = false
    var showReversed  = false
    var traceSteps    = false
    var tracePos      = false
    var doRun         = true
    for (c <- trace.toLowerCase()) c match {
      case 'c' => showCode = true
      case 's' => traceSteps = true
      case '@' => tracePos = true
      case '-' => doRun = false
      case 't' => showTree=true
      case 'r' => showReversed=true
    }
    if (trace.nonEmpty) println(s"$label ($subject) ${pat.source}")
    val compiled = pat.compile(reverse=false, showCode)
    if (showTree) Util.pprint(pat)
    if (showReversed) Util.pprint(pat.reversed)

    if (doRun) {
      val state  = new State[Char](compiled, Groups.empty, subject, start, subject.length, traceSteps)
      result = state.run(reversed=false, search, tracePos)
      for { r <- result} println(s"$label ($subject) ${pat.source}\n=$r ")
    }
  }

  def parse(pat: String): Tree[Char] = new Parser(pat).tree

  def search(trace: String = "", subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    experiment(trace, "search", search=true, subject)(tree)
  }

  def starts(trace: String = "", subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    experiment(trace, "starts", search=false, subject)(tree)
  }


  /** Quadratic findPrefix, using sliding prefix experiment */
  def find(trace: String = "", subject: String)(pat: String): Unit =
  { val tree = new Parser(pat).tree
    var start = 0
    result = None
    while (result.isEmpty && start<subject.length) {
      experiment(trace, "findPrefix", search=false, subject, start)(tree)
      result match {
        case Some(matched) => start = matched.end
        case None          => start += 1
      }
    }
  }

  /** Quadratic allPrefixes, using sliding prefix experiment */
  def findAll(trace: String = "", subject: String)(pat: String): Unit =
  { val tree = new Parser(pat).tree
    var start = 0
    result = None
    while (start<subject.length) {
      experiment(trace, "allPrefixes", search=false, subject, start)(tree)
      result match {
        case Some(matched) => start = matched.end
        case None          => start += 1
      }
    }
  }

  def all(trace: String = "", subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    result    = None
    var start = 0
    var go    = true
    while (go) {
      experiment(trace, "all", search=true, subject, start)(tree)
      result match {
        case Some(matched) => start = matched.end
        case None          => go = false
      }
    }
  }


}
