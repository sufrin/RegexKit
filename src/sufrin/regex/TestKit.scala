package sufrin.regex

object TestKit {
  import sufrin.regex.machine._
  import sufrin.regex.syntax._

  val text = "abcd efg abcdefg"

  import scala.language.postfixOps
  val word   = syntax.Sat((c: Char) => ('a'<=c) && ('z'>=c) , "\\w")
  val unWord = syntax.Sat((c: Char) => ! ('a'<=c) || ! ('z'>=c) , "\\W")

  def run(reversed: Boolean, trace: String = "", label: String, search: Boolean = false, subject: String = text)(pat: Tree[Char]): Unit = {
    var showCode = false
    var showTree = false
    var showReversed = false
    var traceSteps = false
    var tracePos = false
    var doRun = true
    for (c <- trace.toLowerCase()) c match {
      case 'c' => showCode = true
      case 's' => traceSteps = true
      case '@' => tracePos = true
      case '-' => doRun = false
      case 't' => showTree=true
      case 'r' => showReversed=true
    }
    println(s"$label ($subject) ${pat.source}")
    val compiled = pat.compile(reverse=false, showCode)
    if (showTree) Util.pprint(pat)
    if (showReversed) Util.pprint(pat.reversed)

    if (doRun) {
      val state = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
      val result = state.run(reversed, search, tracePos)
      println(s"$label ($subject) ${pat.source} @ ==> ")
      for {r <- result}
        println(s"     =>   $r")
    }
  }

  def parse(pat: String): Tree[Char] = new Parser(pat).tree


  def find(trace: String = "", subject: String = text)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    run(false, trace, "find", search=true, subject)(tree)
  }

  def starts(trace: String = "", subject: String = text)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    run(false, trace, "starts", search=false, subject)(tree)
  }

  def all(trace: String = "", subject: String = text)(pat: String): Unit = {
    var showTree = false
    var showReversed = false
    var showCode   = false
    var traceSteps = false
    var tracePos   = false
    var doRun      = true
    for ( c <- trace.toLowerCase() ) c match {
      case 'c' => showCode=true
      case 's' => traceSteps=true
      case '@' => tracePos=true
      case '-' => doRun=false
      case 't' => showTree=true
      case 'r' => showReversed=true
    }

    var start    = 0
    var running = true
    if (showCode || traceSteps) println(s"all ($subject) ")

    val tree = new Parser(pat).tree
    val compiled = tree.compile(reverse=false, showCode)
    if (showTree) Util.pprint(tree)

    while (running && start<subject.length) {
      var state    = new State[Char](compiled, Groups.empty, subject, start, subject.length, traceSteps)
      var result   = state.run(reversed = false, search=true, tracePos)
      if (result.isEmpty)
        running = false
      else {
        println("********")
        var here = result.get.start
        for {r <- result} println(s"@$start     =>   $r")
        val end = result.get.end
        if (start==end) running = false else start = end
        println("********")
      }
    }
  }

}
