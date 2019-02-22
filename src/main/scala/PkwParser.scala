object PkwParser {
  class Parser[A](val parse: String => List[(A, String)]) {
    def flatMap[B](f: A => Parser[B]): Parser[B] = {
      new Parser[B](input => {
        val bOutListList = for (
          (a, outA) <- this.parse(input)
        ) yield f(a).parse(outA)
        bOutListList.flatten
      })
    }

    def map[B](f: A => B): Parser[B] = {
      flatMap[B](pure[B] _ compose f)
    }
  }

  def pure[A](a: A): Parser[A] = {
    new Parser[A](input => List((a, input)))
  }

  def failure[A](): Parser[A] = new Parser[A](_ => List())

  def Or[A](p: Parser[A])(q: Parser[A]): Parser[A] = {
    new Parser[A](input => {
      p.parse(input) ++ q.parse(input)
    })
  }

  val item: Parser[Char] = new Parser[Char]({
    case "" => List()
    case s => List((s(0), s.substring(1)))
  })

  def sat[A](parser: Parser[A])(predicate: A => Boolean): Parser[A] = {
    parser.flatMap((a: A) => {
      if (predicate(a)) {
        pure(a)
      } else {
        failure()
      }
    })
//    for (
//      a <- parser
//    ) {if (predicate(a)) {pure(a)} else {failure()}}
  }

  val satC: (Char => Boolean) => Parser[Char] = sat(item)
  val char: Char => Parser[Char] = (c: Char) => {satC(c.equals)}
  val digit: Parser[Char] = satC(c => c.isDigit)
  val lower: Parser[Char] = satC(c => c.isLower)
  val upper: Parser[Char] = satC(c => c.isUpper)
  val letter: Parser[Char] = Or(lower)(upper)
  val alphaNum: Parser[Char] = Or(digit)(letter)
  val word: Parser[String] = {
    val step = for ( x <- letter; xs <- word ) yield { x + xs }
    Or(step)(pure(""))
  }
  val string: String => Parser[String] = {
    case "" => pure("")
    case s =>
      val x = s(0)
      val xs = s.substring(1)
      for (_ <- char(x); _ <- string(xs);) yield {
        x + xs
      }
  }
  def many[A](p: Parser[A]): Parser[List[A]] = {
    Or(many1(p))(pure(List()))
  }
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    for ( x <- p; xs <- many(p) ) yield {x::xs}
  }
}
