package chapter9

import _root_.fpinscala.parsing.ParseError
import chapter8.Ex.{SGen, Prop, Gen}

import scala.util.matching.Regex


/**
  * User: bigbully
  * Date: 16/3/1
  * Time: 下午11:48
  */
object Ex extends App{

  case class ParseError(stack:List[(Location, String)])


  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  case class Location(input: String, offset: Int = 0) {

    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int) = copy(offset = offset+n)

    /* Returns the line corresponding to this location */
    def currentLine: String =
      if (input.length > 1) input.lines.drop(line-1).next
      else ""

    def columnCaret = (" " * (col-1)) + "^"
  }


  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  type Parser[+A] = ParseState => Result[A]

  trait Parsers { self =>

    def regex(r: Regex): Parser[String] = {
      val msg = "regex " + r
      s => r.findPrefixOf(s.input) match {
        case None => Failure(s.loc.toError(msg), false)
        case Some(m) => Success(m,m.length)
      }
    }

    def run[A](p:Parser[A])(input:String):Either[ParseError, A]
    def char(c:Char) :Parser[Char] = {
      string(c.toString).map(_.charAt(0))
    }

    def succeed[A](a:A):Parser[A] = {
      string("") map (_ => a)
    }

    //返回parser组成之后的String的parser
    def slice[A](p:Parser[A]):Parser[String] = {
      s => p(s) match {
        case Success(_,n) => Success(s.slice(n),n)
        case f@Failure(_,_) => f
      }
    }

    def or[A](s1:Parser[A], s2: => Parser[A]):Parser[A]

    implicit def string(w:String):Parser[String] = {
      val msg = "'" + w + "'"
      s => {
        val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
        if (i == -1) // they matched
          Success(w, w.length)
        else
          Failure(s.loc.advanceBy(i).toError(msg), i != 0)
      }
    }

    implicit def operators[A](p:Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a:A)(implicit f:A => Parser[String]) : ParserOps[String] = ParserOps(f(a))

    def listOfN[A](n:Int, p:Parser[A]):Parser[List[A]] = {
      if (n < 0) {
        succeed(List())
      }else {
        map2(p, listOfN(n - 1, p))(_ :: _)
      }
    }

    def wrap[A](p: => Parser[A]) :Parser[A]

    def product[A, B](p1:Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
      for(a <- p1; b <- p2) yield (a, b)
    }

    def many[A](p:Parser[A]) :Parser[List[A]] = {
      map2(p, many(p))(_ :: _) or succeed(List())
    }

    def map[A,B](a: Parser[A])(f: A => B): Parser[B] = {
      a.flatMap(a => succeed(f(a)))
    }

    def flatMap[A, B](p:Parser[A])(f:A => Parser[B]):Parser[B]

    def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
      map(product(p, p2))(f.tupled)//把f从接收两个参数转化成接收一个包含两个参数的tuple
    }

    def map2_[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
      for(a <- p; b <- p2) yield f(a, b)
    }

    //返回input中满足parser的零个或多个元素的集合的parser
    def many1[A](p:Parser[A]):Parser[List[A]] = {
      map2(p, many(p))(_ :: _)
    }

    def label[A](msg: String)(p: Parser[A]): Parser[A]

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]


    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    implicit def regex(r:Regex):Parser[String]


    val numA:Parser[Int] = char('a').many.slice.map(_.size)
    run(numA)("aaa")

    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

    val p = label("first magic word")("abra") **
      " ".many **
      label("second magic word")("cadabra")

    case class ParserOps[A](p:Parser[A]) {
      def | [B>:A](p2:Parser[B]):Parser[B] = self.or(p, p2)
      def or [B>:A](p2:Parser[B]):Parser[B] = self.or(p, p2)
      def ** [B](p2:Parser[B]):Parser[(A, B)] = self.product(p, p2)


      def many[A] = self.many(p)

      def many1[A] = self.many1(p)

      def map[B](f:A => B) :Parser[B] = self.map(p)(f)

      def slice[A] = self.slice(p)

      def flatMap[B](f:A =>Parser[B]) = self.flatMap(p)(f)
    }


    object Laws {
      def equal[A](p1:Parser[A], p2:Parser[A])(in:Gen[String]):Prop = {
        Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))
      }

      def mapLaw[A](p:Parser[A])(in:Gen[String]):Prop = {
        equal(p, p.map(a => a))(in)
      }

      def succeedLaw[A](a:A)(in:Gen[String]):Prop = {
        Gen.forAll(in)(s => run(succeed(a))(s) == Right(a))
      }


      def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
      def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

      def productLaw1[A](p1:Parser[A], p2:Parser[A], p3:Parser[A])(in:Gen[String]):Prop = {
        equal(((p1 ** p2) ** p3).map(unbiasL), (p1 ** (p2 ** p3)).map(unbiasR))(in)
      }


      def productLaw2[A, B](p1:Parser[A], p2:Parser[A], f:A => B)(in:Gen[String]):Prop = {
        equal(p1.map(f) ** p2.map(f), (p1 ** p2).map{case (a, b) => (f(a), f(b))})(in)

      }


      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =  {
        SGen.forAll(inputs ** Gen.string) {
          case (input, msg) =>
            run(label(msg)(p))(input) match {
              case Left(e) => errorMessage(e) == msg
              case _ => true
            }
        }
      }
    }
  }


  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON case class JObject(get: Map[String, JSON]) extends JSON


  }






}
