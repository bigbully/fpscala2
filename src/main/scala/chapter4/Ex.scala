package chapter4

import java.util.regex.{PatternSyntaxException, Pattern}

/**
  * User: bigbully
  * Date: 16/1/1
  * Time: 下午4:35
  */
object Ex extends App{


  trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(v) => Some(f(v))
        case None => None
      }
    }
    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }
    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(v) => v
        case None => default
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(Some(_)) getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {
      this.flatMap(a => if (f(a)) Some(a) else None)
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for{
      sa <- a
      sb <- b
    } yield f(sa, sb)
  }

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)


  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher_1(pat1), mkMatcher_1(pat2))((f, g) => f(s) && g(s))
  }

  def sequence_myself[A](a: List[Option[A]]): Option[List[A]] = {
    val l: List[A] = a.foldLeft(List.empty[A]) { (z, a) =>
      a match {
        case Some(v) => v :: z
        case None => z
      }
    }
    if (l.size == a.size) Some(l.reverse) else None
  }

  def sequence[A](a:List[Option[A]]):Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(l => hh :: l))
    }
  }


  def sequence2[A](a:List[Option[A]]):Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((a, z) => map2(a, z)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((a, z) => map2(f(a), z)(_ :: _))
  }


  def sequence3[A](a:List[Option[A]]):Option[List[A]] = {
    traverse(a)(a => a)
  }

//  println(traverse(List(1, 2, 3))(a => {
//    if (a == 5) {
//      None
//    } else {
//      Some(a)
//    }
//  }))
//  println(sequence3(List(Some(1), Some(2), Some(3))))


  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      }
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(v) => f(v)
      }
    }


    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(e) => b
        case s => s
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        r1 <- this
        br1 <- b
      } yield f(r1, br1)
    }

    def sequence[E, A](list:List[Either[E, A]]):Either[E, List[A]] = {
      list match {
        case Nil => Right(Nil)
        case h :: t => h.flatMap(hh => sequence(t).map(l => hh :: l))
      }
    }

    def traverse[E, A, B](list:List[A])(f:A => Either[E, B]):Either[E, List[B]] = {
      list.foldRight[Either[E, List[B]]](Right(Nil))((a, z) => f(a).map2(z)(_ :: _))
    }

    def sequence_2[E, A](list:List[Either[E, A]]):Either[E, List[A]] = {
      traverse(list)(a => a)
    }


  }


  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]




}
