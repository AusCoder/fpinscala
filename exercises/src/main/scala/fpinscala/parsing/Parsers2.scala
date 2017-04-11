package fpinscala.parsing

trait Parsers2[Parser[+_]] { self =>

  def char(c: Char): Parser[Char]

  def string(s: String): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError2, A]

  def or[A](p: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // LAWS

  // run(char(x))(x.toString) == Right(x)
  // run(char('a'))("b") == Left(ParseError2())    possibly with location 0

  // run(char(str))(str) == Right(str)

  // for all str: String and p: Parser[A] such that run(p)(str): Right[A], run(or, p2)(str): Right[A]
}

object StringBasedParser2 {
  // how would I record the location of the failed parsing?
  // can I carry along the number of attempts that I have made in matching?
  type StringFunctionParser[+A] = String => Either[ParseError2, A]

  def ops = new Parsers2[StringFunctionParser] {
    override def char(c: Char): StringFunctionParser[Char] = str =>
      str.headOption.fold[Either[ParseError2, Char]](Left(ParseError2()))(x =>
        if (x == c) Right(x) else char(c)(str)
      )

    override def string(s: String): StringFunctionParser[String] = ???

    override def run[A](p: StringFunctionParser[A])(input: String): Either[ParseError2, A] = ???

    override def or[A](p: StringFunctionParser[A], p2: StringFunctionParser[A]): StringFunctionParser[A] = ???

    override def listOfN[A](n: Int, p: StringFunctionParser[A]): StringFunctionParser[List[A]] = ???
  }
}

// could put a message in here.
case class ParseError2()