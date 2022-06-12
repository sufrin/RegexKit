package sufrin.regex

object TestKit {

  /** Uniform way of showing test results */
  implicit class Showable(a: Any) {
    def show(): Unit =
      a match {
        case i: Iterable[Any] => println("("); for { o <- i } println(s" $o"); println(")")
        case i: Iterator[Any] => println("("); for { o <- i } println(s" $o"); println(")")
        case _                => println(a)
      }
  }

  /**
   *  Evaluates the `thunk` and returns the result paired with
   *  the elapsed time in nanoseconds.
   */
  def timing[A](thunk: => A): (A, Long) = {
    val start = System.nanoTime()
    val r   = thunk
    val end = System.nanoTime()
    val s   = end-start
    (r, s)
  }

  import sufrin.regex.machine._
  import sufrin.regex.syntax._
  import scala.language.postfixOps

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

  val text = """Et licet quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam
               |nixus poterat suppetere liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera
               |simulacra, quae finxere fabulae theatrales.  Haec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse
               |iam conpererat lege communi, scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare
               |otium fere tumultuosum, in eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et
               |Gentilibus, et mandabat Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam
               |properare blande hortaretur et verecunde. Sed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut
               |omnia illa conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret? Et licet
               |quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere
               |liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae
               |theatrales.  Haec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi,
               |scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in
               |eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat
               |Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande
               |hortaretur et verecunde.  Sed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa
               |conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret? Et licet quocumque oculos
               |flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere liberorum, ad usque
               |taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae theatrales. Haec
               |subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi, scribens ad Caesarem
               |blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in eius perniciem
               |conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat Domitiano, ex comite
               |largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande hortaretur et
               |verecunde.  Sed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa conficiat, quid ego,
               |senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret.""".stripMargin

}
