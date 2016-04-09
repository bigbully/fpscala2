package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.annotation.tailrec

/**
  * User: bigbully
  * Date: 16/2/20
  * Time: 下午10:53
  */
object ExNonBlocking extends App{

  sealed trait Future[A] {
    private[chapter7] def apply(cb: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es:ExecutorService)(p:Par[A]):A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es){a => ref.set(a); latch.countDown()}
      latch.await()
      ref.get()
    }

    def unit[A](a:A):Par[A] = {
      es => new Future[A] {
        override def apply(k: (A) => Unit): Unit = k(a)
      }
    }

    def fork[A](a: => Par[A]):Par[A] = {
      es => new Future[A] {
        override def apply(cb: (A) => Unit): Unit = {
          eval(es)(a(es)(cb))
        }
      }
    }

    def eval(es:ExecutorService)(r: => Unit):Unit = {
      es.submit(new Runnable {
        override def run(): Unit = r
      })
    }

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = {
      es => new Future[C] {
        override def apply(cb: (C) => Unit): Unit = {
          var ar:Option[A] = None
          var br:Option[B] = None
          val combiner = Actor[Either[A, B]](es){
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }


          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))


    def flatMap[A, B](p:Par[A])(f:A => Par[B]) :Par[B] = {
      es => new Future[B] {
        override def apply(cb: B => Unit): Unit =
          p(es)(a =>f(a)(es)(cb))
      }
    }

    def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] = flatMap(p)(f)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      flatMap(n)(i => choices(i))
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      flatMap(cond)(if (_) t else f)
    }

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
      flatMap(key)(choices(_))
    }

    def join[A](p: Par[Par[A]]): Par[A] = {
      es => new Future[A] {
        override def apply(cb: (A) => Unit): Unit = {
          p(es)(p2 => eval(es)(p2(es)(cb)))
        }
      }
    }


    def flatMap_[A, B](p:Par[A])(f:A => Par[B]) :Par[B] = {
      join(map(p)(f))
    }

    def join_[A](p: Par[Par[A]]): Par[A] = {
      flatMap(p)(pa => pa)
    }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }

}

final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
  self =>

  private val tail = new AtomicReference(new Node[A]())
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  /** Alias for `apply` */
  def !(a: A) {
    val n = new Node(a)
    head.getAndSet(n).lazySet(n)
    trySchedule()
  }

  /** Pass the message `a` to the mailbox of this actor */
  def apply(a: A) {
    this ! a
  }

  def contramap[B](f: B => A): Actor[B] =
    new Actor[B](strategy)((b: B) => (this ! f(b)), onError)

  private def trySchedule() {
    if (suspended.compareAndSet(1, 0)) schedule()
  }

  private def schedule() {
    strategy(act())
  }

  private def act() {
    val t = tail.get
    val n = batchHandle(t, 1024)
    if (n ne t) {
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1)
      if (n.get ne null) trySchedule()
    }
  }

  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] = {
    val n = t.get
    if (n ne null) {
      try {
        handler(n.a)
      } catch {
        case ex: Throwable => onError(ex)
      }
      if (i > 0) batchHandle(n, i - 1) else n
    } else t
  }
}

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

object Actor {

  /** Create an `Actor` backed by the given `ExecutorService`. */
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
    new Actor(Strategy.fromExecutorService(es))(handler, onError)
}

/**
  * Provides a function for evaluating expressions, possibly asynchronously.
  * The `apply` function should typically begin evaluating its argument
  * immediately. The returned thunk can be used to block until the resulting `A`
  * is available.
  */
trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {

  /**
    * We can create a `Strategy` from any `ExecutorService`. It's a little more
    * convenient than submitting `Callable` objects directly.
    */
  def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val f = es.submit { new Callable[A] { def call = a} }
      () => f.get
    }
  }

  /**
    * A `Strategy` which begins executing its argument immediately in the calling thread.
    */
  def sequential: Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}
