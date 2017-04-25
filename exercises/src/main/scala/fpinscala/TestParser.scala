package fpinscala

import fpinscala.parsing.{Parsers2, ParseError2, StringBasedParser2}

object TestParser {
  def main(args: Array[String]): Unit = {
    val charP = StringBasedParser2.ops.char('k')
    val stringP = StringBasedParser2.ops.string("hello")

    val x = StringBasedParser2.ops.run(charP)("akbc")
    val y = StringBasedParser2.ops.run(stringP)("hello world")

    println(x)
    println(y)
  }
}
