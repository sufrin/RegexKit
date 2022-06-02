
import sufrin.regex.syntax.lexer._

val l1 = new Lexer("(foo|bar)*")
val l2 = new Lexer("""(foo|bar\uabcd)*""")
val l3 = new Lexer("""(foo|bar\n)*""")
val l4 = new Lexer("""(\w+\W|\d+\D)**""")

val l5 = new Lexer("[^a-zpq-r]")
val l6 = new Lexer("[pq-rs]")
val l7 = new Lexer("[^a-zpq-r]abcdefg")
val l8 = new Lexer("[^a-z[pq-r]]abcdefg")
val l9 = new Lexer("[^a-z&&[pq-r]]abcdefg")
