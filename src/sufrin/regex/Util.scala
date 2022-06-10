package sufrin.regex

import sufrin.regex.syntax.{Parser, Tree}

object Util {

  def isPrim(obj: Any): Boolean =
    obj match {
      case _ : Int | _ : Long | _ : Char | _ : String | _ : Double | _ : Float => true
      case _ => false
    }

  def isSingleton(obj: Any): Boolean =
    obj match {
      case prod : Product => prod.productArity==1 && isPrim(prod.productElement(0))
      case _              => false
    }

  def pprint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {
    val indent = ("\u2502 ") * depth + "\u2514\u2500"
    val prettyName = paramName.fold("")(x => s"$x: ") // name: or ""

    val ptype = obj match {
      case _:   Iterable[Any] =>
      case obj: Product       => obj.productPrefix
      case _                  => obj.toString
    }

    print(s"$indent$prettyName$ptype")
    if (!isSingleton(obj)) println()

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(pprint(_, depth + 1))

      case obj: Product  =>
        if (isSingleton(obj))
          println(s"(${obj.productElement(0)})")
        else
          (obj.productIterator zip obj.productElementNames).foreach {
            case (subObj, paramName) => pprint (subObj, depth + 1, Some (paramName) )
          }

      case _ =>
    }
  }

  def parse(pat: String): Tree[Char] = new Parser(pat).tree

}
