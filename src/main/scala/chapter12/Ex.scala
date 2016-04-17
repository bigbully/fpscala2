package chapter12

import chapter11.Ex.Functor

/**
  * 功能描述
  * Created by 晋助 on 16/4/16.
  */
class Ex extends App{

  trait Applicative[F[_]] extends Functor[F] {

    def apply[A, B](fab:F[A => B])(fa:F[A]):F[B]

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      apply[A, B](unit(f))(fa)
    }

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      apply[B, C](map(fa)(f.curried))(fb)
    }

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
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def apply[A, B](fab: Either[E, (A) => B])(fa: Either[E, A]): Either[E, B] = ???

    override def unit[A](a: => A): Either[E, A] = ???
  }


}
