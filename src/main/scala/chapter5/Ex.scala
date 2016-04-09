package chapter5

import scala.annotation.tailrec
import chapter5.Ex.Stream.cons
import scala.collection.mutable.ListBuffer

/**
  * User: bigbully
  * Date: 16/1/17
  * Time: 上午9:35
  */
object Ex extends App{


  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toListRecursive: List[A] = {
      this match {
        case Empty => List.empty[A]
        case Cons(head, stream) => head() :: stream().toListRecursive
      }
    }


    def toList: List[A] = {

      @tailrec
      def go(stream:Stream[A], result:List[A]) :List[A] = {
        stream match {
          case Empty => result
          case Cons(head, tail) => go(tail(), head() :: result)
        }
      }
      go(this, List()).reverse
    }

    def toListFast: List[A] = {
      val buf = new ListBuffer[A]()
      @tailrec
      def go(stream:Stream[A]) :Unit = {
        stream match {
          case Empty =>
          case Cons(head, tail) => {
            buf += head()
            go(tail())
          }
        }
      }
      go(this)
      buf.toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n > 0 => tail().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
      case _ => Stream()
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
        case _ => z
      }
    }

    def exists2(f: A => Boolean) : Boolean =
      foldRight(false)(f(_) || _)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)(p(_) && _)

    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Empty:Stream[A])((h, t) => if(p(h)) cons(h, t) else Empty)

    def headOption2: Option[A] =
      foldRight(None:Option[A])((h, _) => Some(h))


    def map[B](f: A => B):Stream[B] = {
      foldRight(Stream[B]())((h, stream) => cons(f(h), stream))
    }

    def filter(f: A => Boolean):Stream[A] = {
      foldRight(Stream[A]())((h, stream) => if (f(h)) cons(h, stream) else stream)
    }

    def append[B >: A](stream: => Stream[B]) :Stream[B] = {
      foldRight(stream)((h, t) => cons(h, t))
    }

    def flatMap[B](f: A => Stream[B]):Stream[B] = {
      foldRight(Stream[B]())((h, stream) => f(h).append(stream))
    }

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def map2[B](f: A => B):Stream[B] = {
      Stream.unfold(this){
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    }

    def take2(n: Int): Stream[A] = {
      Stream.unfold((this, n)) {
        case (Cons(h, t), n) if n == 1 => Some((h(), (Stream.empty, n - 1)))
        case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
        case _ => None
      }
    }

    def takeWhile3(p: A => Boolean): Stream[A] = {
      Stream.unfold(this) {
        case Cons(h, t) =>{
          lazy val head = h()
          if (p(head))
            Some((head, t()))
          else
            None
        }
        case _ => None
      }
    }

    def zip[B](s2: Stream[B]): Stream[(A,B)] =
      zipWith(s2)((_,_))

    def zipWith[B, C](s:Stream[B])(add:(A, B)=> C):Stream[C] = {
      Stream.unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((add(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
      zipWithAll(s2)((_, _))
    }

    def zipWithAll[B, C](s2:Stream[B])(f:(Option[A], Option[B]) => C) : Stream[C] = {
      Stream.unfold((this, s2)) {
        case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (Empty, t2()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
        case _ => None
      }
    }

    def startsWith[A](sub: Stream[A]): Boolean = {
      zipAll(sub).takeWhile3(!_._2.isEmpty).forAll{
        case (h1, h2) => h1 == h2
      }
    }

    def tails: Stream[Stream[A]] = {
      Stream(this) append Stream.unfold(this){
        case Cons(h, t) => Some((t(), t()))
        case _ => None
      }
    }

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    def scanRight[B >: A](z:B)(f: (A, => B) => B):Stream[B] = {
      foldRight((z, Stream(z))) { (a, p0) => {
        lazy val p1: (B, Stream[B]) = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      }
      }._2
    }

  }



  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t:() => Stream[A]) extends Stream[A]



  object Stream {
    def empty[A]: Stream[A] = Empty


    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = {
      lazy val stream:Stream[A] = cons(a, stream)
      stream
    }

    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    def fibs():Stream[Int] = {

      def go(n1:Int, n2:Int):Stream[Int] = {
        cons(n1, go(n2, n1 + n2))
      }

      go(0, 1)
    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((v, nextState)) => {
          cons(v, unfold(nextState)(f))
        }
        case None => Stream.empty
      }
    }

    def constant2[A](a: A): Stream[A] = {
      unfold(a)(_ => Some((a, a)))
    }

    def from2(n: Int): Stream[Int] = {
      unfold(n)(n => Some(n, n+1))
    }

    def fibs2():Stream[Int] = {
      unfold((0, 1)) {
        case (s1, s2) => Some((s1, (s2 , s1+ s2)))
      }
    }

  }


  val ones: Stream[Int] = Stream.cons(1, ones)
//  println(ones.take(5).toList)
  val s = Stream.from(10)

  println(Stream(1,2,3).scanRight(0)(_ + _).toList)
//  println(Stream(1, 2, 3, 4).takeWhile2(a => a < 3 && a > 1).toList)


}
