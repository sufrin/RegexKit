import sufrin.regex.machine._
import sufrin.regex.syntax
import sufrin.regex.syntax._

val text = "abcd efg abcdefg"

import scala.language.postfixOps
val word   = syntax.Sat((c: Char) => ('a'<=c) && ('z'>=c) , "\\w")
val unWord = syntax.Sat((c: Char) => ! ('a'<=c) || ! ('z'>=c) , "\\W")

def run(trace: String = "", label: String, search: Boolean = false, subject: String = text)(pat: Tree[Char]): Unit =
  { var showCode   = false
    var traceSteps = false
    var tracePos   = false
    for ( c <- trace.toLowerCase() ) c match {
      case 'c' => showCode=true
      case 's' => traceSteps=true
      case '@' => tracePos=true
    }
    if (showCode||traceSteps) println(label)
    val compiled = pat.compile(showCode)
    val state    = new State[Char](compiled, Groups.empty, subject, 0, subject.length, traceSteps)
    val result   = state.run(search, tracePos)
    println(s"$label ${pat.source} @ $subject ==> ")
    for { r <- result }
      println(s"     =>   $r")
  }


def parse(pat: String): Tree[Char] = new Parser(pat).tree


def find(trace: String = "", subject: String = text)(pat: String): Unit = {
  val tree = new Parser(pat).tree
  run(trace, "find", search=true, subject)(tree)
}

def starts(trace: String = "", subject: String = text)(pat: String): Unit = {
  val tree = new Parser(pat).tree
  run(trace, "starts", search=true, subject)(tree)
}

def all(trace: String = "", subject: String = text)(pat: String): Unit = {
  var showCode   = false
  var traceSteps = false
  var tracePos   = false
  for ( c <- trace.toLowerCase() ) c match {
    case 'c' => showCode=true
    case 's' => traceSteps=true
    case '@' => tracePos=true
  }
  val tree = new Parser(pat).tree
  var start    = 0
  var running = true
  println(s"all ($subject) $pat ==> ")

  val compiled = tree.compile(showCode)

  while (running && start<subject.length) {
    var state    = new State[Char](compiled, Groups.empty, subject, start, subject.length, traceSteps)
    var result   = state.run(search=true, tracePos)
    if (result.isEmpty)
      running = false
    else {
      println("********")
      var here = result.get.start
      for {r <- result} println(s"@$start     =>   $r")
      start = result.get.end
      println("********")
    }
  }
}

def span(t: Tree[Char]): Tree[Char] = Span(false, false, t)
//run("", "run")(Alt("abc"!, span("abcdef"!)))
starts("")("abc|(abcdef)")
println("============")
find("")("abc|(abcdef)")
println("============")
all("")("abc|(abcdef)")
println("============")
all("")("efg|(abcdef)")
println("============")
all("", "the bcd representxy is foxed")("bcd|(efg$)|xy|(abcdef)")
println("============")
all("@cs", "ten is  10 andnext is 123")("(\\d+)(\\D|$)")
println("============")
all("@cs", "ten is 10 next is 123")("\\D(\\d+)")
println("============")
all("@cs", "ten is 10 next is 123 ")("\\D(\\d+)")
println("============")
find("@cs", "fooabbggggggcde")("(x? | a?b)+bg*?cd")

