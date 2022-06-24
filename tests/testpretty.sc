import sufrin.regex.PrettyPrint._

Some(3).prettyPrint()
Some((Some(3))).prettyPrint()
Some(List(1->"foo", 2-> "bar", 3 -> "bang")).prettyPrint()
Some(List(1->"foo", 2-> "bar", 3 -> Some(4))).prettyPrint()
sufrin.regex.Regex("abc|def*|[^gh]").tree.prettyPrint()

class Foo(val a: Int, b: Int, c: List[Int] = List(4,5,6)) extends PrettyPrintable {
  def prefix: String = "Foo"
  def arity = 3

  override def field(i: Int): (String, Any) = i match {
    case 0 =>  ("a", a)
    case 1 =>  ("b", b)
    case 2 =>  ("c", c)
  }
}


Some(List(1, 2, new Foo(3,5))).prettyPrint()

