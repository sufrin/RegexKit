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

  case class Source(pat: Tree[Char], showCompiled: Boolean) {
    lazy val compiled = pat.compile(reverse=false, showCompiled)
  }


  var result: Option[Match[Char]] = None

  def experiment(trace: String, label: String, search: Boolean = false, subject: String, start: Int = 0)(pat: Source): Unit = {
    var showCode      = false
    var showTree      = false
    var showReversed  = false
    var traceSteps    = false
    var tracePos      = false
    var doRun         = true
    var noSubject     = false
    for (c <- trace.toLowerCase()) c match {
      case 'c' => showCode = true
      case 's' => traceSteps = true
      case '@' => tracePos = true
      case '-' => doRun = false
      case 't' => showTree=true
      case 'r' => showReversed=true
      case 'n' => noSubject=true
    }
    if (label.nonEmpty) println(s"$label: $subject")
    val compiled = pat.compiled
    if (showTree) PrettyPrint.prettyPrint(pat)
    if (showReversed) PrettyPrint.prettyPrint(pat.pat.reversed)

    if (doRun) {
      val state  = new State[Char](compiled, Groups.empty, subject, start, subject.length, traceSteps)
      result = state.run(reversed=false, search, tracePos)
      for { r <- result} println(s" => $r ")
    }
  }

  def parse(pat: String): Tree[Char] = new Parser(pat).tree

  def search(trace: String = "", subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    experiment(trace, s"search: $pat", search=true, subject)(Source(tree, trace contains 'c'))
  }

  def starts(trace: String = "", subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    experiment(trace, s"starts: $pat", search=false, subject)(Source(tree, trace contains 'c'))
  }



  /** Quadratic findPrefix, using sliding prefix experiment */
  def find(trace: String, subject: String)(pat: String): Unit =
  { val tree = new Parser(pat).tree
    var start = 0
    result = None
    while (result.isEmpty && start<subject.length) {
      experiment(trace, s"find: $pat", search=false, subject, start)(Source(tree, trace contains 'c'))
      result match {
        case Some(matched) => start = matched.end
        case None          => start += 1
      }
    }
  }

  /** Quadratic allPrefixes, using sliding prefix experiment */
  def findAllTrees(trace: String, subject: String)(pat: Tree[Char]): Unit =
  { var start = 0
    result = None
    val patSource = Source(pat, trace contains 'c')
    while (start<subject.length) {
      experiment(trace, "", search=false, subject, start)(patSource)
      result match {
        case Some(matched) => start = matched.end
        case None          => start += 1
      }
    }
  }

  def findAll(trace: String, subject: String)(pat: String): Unit =
      findAllTrees(trace, subject)(new Parser(pat).tree)

  def findAllBranches(trace: String, subject: String)(pats: String*): Unit = {
    val trees = for { pat <- pats }  yield  new Parser(pat).tree
    val tree = Branched(trees)
    findAllTrees(trace, subject)(tree)
  }

  def all(trace: String, subject: String)(pat: String): Unit = {
    val tree = new Parser(pat).tree
    result    = None
    var start = 0
    var go    = true
    while (go) {
      experiment(trace, s"all: $pat", search=true, subject, start)(Source(tree, trace contains 'c'))
      result match {
        case Some(matched) => start = matched.end
        case None          => go = false
      }
    }
  }

  val text = """Et licet quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam
               |nixus poterat suppetere liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera
               |simulacra, quae finxere fabulae theatrales.  Haec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse
               |iam conpererat lege communi, (scribens ad Caesarem) blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare
               |otium fere tumultuosum, in (eius perniciem (conspiraret, solisque) scholis iussit esse contentum) palatinis et protectorum cum Scutariis et
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
