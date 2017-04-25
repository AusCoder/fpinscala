package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]



  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = ???
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}