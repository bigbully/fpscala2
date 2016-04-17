package chapter11

import chapter6.Ex.State
import chapter7.ExNonBlocking.Par
import chapter8.Ex.Gen
import chapter9.Ex.{Parser, Parsers}
import chapter5.Ex.Stream

/**
  * User: bigbully
  * Date: 16/4/3
  * Time: 下午4:49
  */
object Ex extends App{

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = {
      (map(fab)(_._1), map(fab)(_._2))
    }

    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
    }


  }

  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }


  trait Monad[F[_]] extends Functor[F]{
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a,b)))

    override def map[A, B](fa:F[A])(f:A => B) :F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }

    def sequence[A](lma:List[F[A]]):F[List[A]] = {
      lma.foldLeft(unit(List[A]()))((fz, fa) => map2(fa, fz)(_::_))
    }

    def traverse[A, B](la:List[A])(f:A => F[B]) :F[List[B]] = {
      la.foldLeft(unit(List[B]()))((fz, a) => map2(f(a), fz)(_::_))
    }

    def replicateM[A](n:Int, ma:F[A]):F[List[A]] = {
      sequence(List.fill(n)(ma))
    }

    def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))


    def flatMap_[A, B](fa:F[A])(f:A => F[B]):F[B] = {
      compose((_:Unit) => fa, f)(())
    }


    def flatMap__[A, B](fa:F[A])(f:A => F[B]):F[B] = {
      join(map(fa)(f))
    }

    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
      a => flatMap(f(a))(g)
    }

    def compose__[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
      a => join(map(f(a))(g))
    }


    def filterM_[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms.foldRight(unit(List[A]()))((x,y) =>
        compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      ms.foldLeft(unit(List[A]()))((fz, a) => map2(f(a), fz)((bool, z) => if (bool) a:: z else z))
    }


    def join[A](mma: F[F[A]]): F[A] = {
      flatMap(mma)(fa => fa)
    }



    def unit[A](a : => A) :F[A]
    def flatMap[A, B](fa:F[A])(f:A => F[B]):F[B]

  }

  object Monad {
    val genMonad = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)

      override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
    }

    val parMonad = new Monad[Par]{
      override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

      override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap_(fa)(f)
    }

    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)
    }

    val streamMonad = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream(a)

      override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa.flatMap(f)
    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
    }


    class StateMonads[S] {
      type StateS[A] = State[S, A]

      val monad = new Monad[StateS] {
        override def unit[A](a: => A): StateS[A] = State.unit(a)

        override def flatMap[A, B](fa: StateS[A])(f: (A) => StateS[B]): StateS[B] = fa.flatMap(f)
      }
    }

    def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)

      override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
    }

    case class Order(item: Item, quantity: Int)
    case class Item(name: String, price: Double)

    val genOrder:Gen[Order] = for{
      item <- genItem
      quantity <-Gen.choose(1, 100)
    }yield Order(item, quantity)

    val genItem: Gen[Item] = for {
      name <- Gen.stringN(3)
      price <- Gen.uniform.map(_ * 10)
    } yield Item(name, price)

    case class Id[A](value:A) extends Monad[Id[A]]{
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = {
        f(fa.value)
      }


    }

  }



}
