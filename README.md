# RegexKit

## Introduction

`RegexKit` provides an implementation of (more or less) standard
regular expression matching and group capture, forwards or backwards,
over subsequences of sequences of arbitrary type. It was implemented
because the standared Java/Scala API was too restrictive for my
application -- a text editor.

Its `CharSequence` matching facilities are well-developed and tested
(genericity came as an unplanned bonus).  **There is no backtracking
for anchored matches** which -- in the case of a failure -- take
bounded time:  proportional to the product of the 'lengths' of the
regular expression and the subject subsequence.

The principal methods of the API are the matching and searching methods
of `Regex` that act on specified subsequences of the given subject:

```
  // anchored methods
  def suffixes   (subject: CharSequence, from: Int, to: Int,   bound: Int): Option[StringMatch] 
  def matches    (subject: CharSequence, from: Int, to: Int,   bound: Int): Option[StringMatch] 

  // searching methods
  def findPrefix (subject: CharSequence, from: Int, to: Int,   bound: Int): Option[StringMatch] 
  def findSuffix (subject: CharSequence, from: Int, to: Int,   bound: Int): Option[StringMatch]

  // repeated searches
  def allPrefixes(subject: CharSequence, _from: Int, _to: Int, bound: Int): Iterator[StringMatch] 
  def allSuffixes(subject: CharSequence, _from: Int, _to: Int, bound: Int): Iterator[StringMatch]

```

and its substitution method, that substitutes an expanded instance
of the template for each matching instance of this regular expression
in the input. Return the substituted result with a count of the
number of substitutions that were made. When `literal` is true, the
template is not expanded.

```
  def substituteAll(subject: CharSequence, template: String, literal: Boolean, bound: Int): (Int, String)
```

An individual `StringMatch` has a substitute method that replaces all occurences of `$$` in the template with `$`,
and all occurences of `$`*i* (when *i* is a digit) with `group(`*i*`).getOrElse("")`.

```
   def substitute(template: String): String
```

The API also provides methods for scanning forwards and backwards for (properly-nested) bracketed text,
whose opening and closing brackets are specified by regular expressions.

## Implementation method

Regular expressions are translated into code for an abstract
"recognition machine". During matching, the machine simulates a
nondeterministic finite state machine by running simulated deterministic
machines (we call them *threads*) in pseudo-parallel: one for each
unrefuted potential match (a deterministic match is refuted by an
input that fails to match an outgoing edge from its current state).

Subject sequences are offered element by element to the machine and
each active *thread* either accepts, refutes, or moves to its next
state.

The translation from regular expression to recognition machine code is
straightforward, and no attempt is made to tarnsform the expression
into a more efficient form, *or to a form that cannot
(under some circumstances) cause non-termination*. With this in mind, the
matching and searching methods may be given an upper bound to
the number of recognition machine cycles they execute that
causes them to abort if it is exceeded. *This is a pragmatic and expedient
solution to the performance issues that would arise from an attempt to
detect non termination: we find a more efficient method of non-termination
detection in due course; but it's not a priority for us.*

## Examples

The following examples are abstracted, somewhat edited, from the source code
of the `Red` text editor.

Patterns matching the left and right boundaries of various granularities
of text lump.

```

   object Boundaries {
    import sufrin.regex.Regex
    val leftWord  : Regex   = Regex("""\W\w""")
    val rightWord : Regex   = Regex("""\w\W""")
    val leftLine  : Regex   = Regex("""(\n|^)[^\n]*""")
    val rightLine : Regex   = Regex.literal("\n") 
    val leftPara  : Regex   = Regex("(\n|^)\\s*\n")
    val rightPara : Regex   = Regex("\n\\s*(\n|$)")
   }

```



This method performs a search, around the `start`, for a "chunk" (a word, line, or
paragraph) whose left and right boundaries are specified by the
regular expressions `l` and `r`. It returns the left and right
boundaries of the chunk.

```

def selectChunk(start: Int, l: Regex, r: Regex, adjl: Int, adjr: Int): Option[(Int, Int)] =
    // find the last suffix matching l that ends no later than start
    l.findSuffix(document.characters, 0, start) match {
      // no such suffix
      case None => None
      // suffix extends from lp.start to lp.end
      // find the next prefix that starts no earlier than lp.end
      case Some(lp) =>
        r.findPrefix(document.characters, lp.end, document.characters.length) match {
          // no such suffix
          case None => None
          // suffix extends from rp.start to rp.end
          // chunk extends from lp.start to rp.end
          case Some(rp) =>
            val (start, end) =
                (if (lp.start == 0) lp.start else lp.start + adjl, rp.end - adjr)
            Some((start, end))
        }
    }
```

This method performs an unanchored search from the current cursor
position in the indicated direction.

```

  def find(pattern: String, backwards: Boolean, literal: Boolean): Boolean = {
      val chars = document.characters
      try {
        val regex = if (lit) Regex.literal(pattern) else Regex(pattern)
        if (backwards) {
          val lastMatch = regex.findSuffix(chars, 0, cursor, Utils.stepLimit)
          lastMatch match {
            case None           => feedback.notify("Not found upwards", s"$pattern")
                                   false
            case Some(instance) => cursor = instance.start
                                   setMark(instance.end)
                                   true
          }
        } else {
          val nextMatch = regex.findPrefix(chars, cursor, chars.length, Utils.stepLimit)
          nextMatch match {
            case None           => feedback.notify("Not found", s"$pattern")
                                   false
            case Some(instance) => cursor = instance.end min (chars.length-1)
                                   setMark(instance.start)
                                   true
          }
        }
      } catch { case exn: sufrin.regex.syntax.lexer.SyntaxError =>
                notify("Find", s"Pattern: "$pattern" is ill-formed\n${exn.getMessage}")
                false }
    }
  
```
          


The search is for `pattern` interpreted either as a literal or as a regular
expression. The cursor is moved to the start of the match, and the *mark*
(the other end of the selection) is moved to the opposite end.

        Bernard Sufrin
        Oxford, 2020

