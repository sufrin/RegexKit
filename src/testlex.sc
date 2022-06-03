
import sufrin.regex.syntax.lexer._

var tracing = false

def trial(text: String): Unit =
  {
    try {
      val l = new Lexer(text, tracing)
      println(s"$text => ${l.toList}")
    }
    catch {
      case exn: SyntaxError => println(exn.getMessage)
    }
  }

trial("(foo|bar)*")

trial("""(foo|bar\uabcd)*""")

trial("""(foo|bar\n)*""")

trial("""(\w+\W|\d+\D)*?""")

trial("[^a-zpq-r]")

trial("[pq-r^s]")

trial("[^a-zpq-r]abc[^d]efg")

trial("[^a-z[pq-r]]abcdefg")

trial("[^a-z&&[pq-r]]|abcdefg")

trial("[foo^")
trial("[foo^]")
trial("[foo^c]")
trial("]")
trial("[^]]")
trial("[a-x^]]")
trial("[a-x^\\]]")
trial("[a-x\\^\\]]")
trial("a&b")
trial("[a&b]")
trial("[a\\&b]")










