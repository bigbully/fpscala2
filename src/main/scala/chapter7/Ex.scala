package chapter7

import java.util.concurrent.{ExecutorService, Callable, TimeUnit}

import scala.concurrent.duration.TimeUnit
import scala.concurrent.duration.TimeUnit


/**
  * User: bigbully
  * Date: 16/2/16
  * Time: 下午10:38
  */
object Ex extends App{


  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }







  object Par {

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
      a(s)
    }

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f:(A, B) => C) extends Future[C] {

      @volatile var cache: Option[C] = None

      override def get: C = compute(Long.MaxValue)

      override def isCancelled: Boolean = a.isCancelled || b.isCancelled

      override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

      override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

      override def isDone: Boolean = cache.isDefined

      private def compute(timeoutMs: Long): C = cache match {
        case Some(c) => c
        case None => {
          val start = System.currentTimeMillis()
          val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
          val stop = System.currentTimeMillis()
          val at = stop - start
          val br = b.get(at, TimeUnit.MILLISECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
        }
      }
    }

    def map2_[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout:Long, unit:TimeUnit): Par[C] = {
      es => {
        val (af, bf)= (a(es), b(es))

        Map2Future(af, bf, f)
      }
    }


    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
      es => {
        val af = a(es)
        val bf = b(es)

        UnitFuture(f(af.get, bf.get))
      }
    }

    def fork[A](a: => Par[A]):Par[A] = {
      es => es.submit(new Callable[A] {
        override def call: A = a(es).get
      }).asInstanceOf[Future[A]]
    }

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)



    def asyncF[A,B](f: A => B): A => Par[B] = {
      a => fork(unit(f(a)))
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
      map(parList)(_.sorted)
    }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))


    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List[A]()))((pa, z) => map2(pa, z)((a, list) => a :: list))
    }

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p,p2)(_ == _)

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence_[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)


    def parMap[A, B](ps:List[A])(f:A => B):Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }


    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val lpl: List[Par[List[A]]] = as.map(a => asyncF((a:A) => if (f(a)) List(a) else List())(a))
      map(sequence_(lpl))(_.flatten)
    }




  }




}
