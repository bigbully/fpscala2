package chapter12

import chapter11.Ex.Functor

/**
  * 功能描述
  * Created by 晋助 on 16/4/16.
  */
class Ex extends App{

  trait Applicative[F[_]] extends Functor[F] {

    def apply[A, B](fab:F[A => B])(fa:F[A]):F[B] = {
      map2(fab, fa)(_(_))
    }

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      map2(fa, unit(()))((a, _) => f(a))
    }

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
//    {
//      apply[B, C](map(fa)(f.curried))(fb)
//    }

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
    }

    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      fas.foldRight(unit(List[A]()))((a, fbs) => map2(a, fbs)(_::_))
    }

    def sequence_[A](fas: List[F[A]]): F[List[A]] = {
      traverse(fas)(a => a)
    }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
      sequence(List.fill(n)(fa))
    }

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
      map2(fa, fb)((_, _))
    }

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      apply(apply(apply[A, B => C => D](unit(f.curried))(fa))(fb))(fc)
    }


    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                        fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      apply(apply(apply(apply[A, B => C => D => E](unit(f.curried))(fa))(fb))(fc))(fd)
    }

    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) = {
          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
        }

      }
    }
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  def eitherMonad[E] = new chapter11.Ex.Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = {
      fa match {
        case Right(v) => f(v)
        case _ => _
      }
    }
  }


  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationMonad[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(v1), Success(v2)) => Success(f(v1, v2))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

}
