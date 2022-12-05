


sufrin.regex.Template("foobaz is $1 best $for you")
sufrin.regex.Template("foobaz + is/$1/best $for you")
sufrin.regex.Template("foobaz + is/$192foo/best $for you")
sufrin.regex.Template("foo$---baz + is/$192foo/best $for you")
sufrin.regex.Template("foo$$baz + is/$192foo/best $for you")

val r = new sufrin.regex.Rewrite.Rules(List(
    "([A-Z]+)" -> "/$0/",
    "([0-9]+)" -> "#$0",
    "foo"      -> "bar",
    "bar"      -> "foo",
    "([a-z]+)" -> "[$0]",
    "(.)"      -> "$0"
))

r("THE RAIN IN 43 SPAIN \nis foonly barter\n HOORAY")

val wrong = new sufrin.regex.Rewrite.Rules(List(
    "([A-Z]+)" -> "/$0/",
    "([0-9]+)" -> "#$0",
    "foo"      -> "bar$1",
    "bar"      -> "foo",
    "([a-z]+)" -> "[$2]",
    "(.)"      -> "$0"
))