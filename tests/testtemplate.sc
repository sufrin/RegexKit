import TestKit._


sufrin.regex.Template("foobaz is $1 best $for you").toStruct.show()
sufrin.regex.Template("foobaz + is/$1/best $for you").toStruct.show()
sufrin.regex.Template("foobaz + is/$192foo/best $for you").toStruct.show()
sufrin.regex.Template("foo$---baz + is/$192foo/best $for you").toStruct.show()
sufrin.regex.Template("foo$$baz + is/$192foo/best $for you").toStruct.show()

val r = new sufrin.regex.Rewrite.Rules(List(
    "([A-Z]+)" -> "/$0/",
    "([0-9]+)" -> "#$0",
    "mainly"   -> "somewhat",
    "somewhat" -> "mainly",
    "([a-z]+)" -> "[$0]",
    "(.)"      -> "$1"  // all else is text; but see below
))

val s = new sufrin.regex.Rewrite.Rules(List(
    "([A-Z]+)" -> "/$1/",
    "([0-9]+)" -> "#$1",
    "mainly"   -> "somewhat",
    "somewhat" -> "mainly",
    "([a-z]+)" -> "[$1]",
    ".?"       -> "$0"  // WHY does "." here not behave the same as (.) or .?
))

r.show()

r("THE RAIN IN 43 SPAIN \nfalls somewhat but not ''mainly'' IN THE\nPLAIN").show()

s.show()

s("THE RAIN IN 43 SPAIN \nfalls somewhat but not ''mainly'' IN THE\nPLAIN").show()

"Expecting an IallegalArgumentException caused by 'unsound rules' :".show()
val wrong = new sufrin.regex.Rewrite.Rules(List(
    "([A-Z]+)" -> "/$0/",
    "([0-9]+)" -> "#$0",
    "foo"      -> "bar$1", // unsound grpoup reference
    "bar"      -> "foo",
    "([a-z]+)" -> "[$2]", // unsound group reference
    "(.)"      -> "$0"
))