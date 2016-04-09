package chapter8


import java.util.concurrent.{ExecutorService, Executors}

import chapter6.Ex.{RNG, SimpleRNG, State}
import chapter7.Ex.Par.Par
import chapter7.Ex.Par
import chapter8.Ex.Prop._
import chapter8.Ex.{Gen, SGen}
import SGen._
import Gen._

/**
  * User: bigbully
  * Date: 16/2/21
  * Time: 下午8:00
  */
object Ex extends App{

  case class SGen[+A](g: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = g(n)

    def map[B](f: A => B): SGen[B] =
      SGen(g andThen (i => i map f))

    def flatMap[B](f: A => Gen[B]): SGen[B] =
      SGen(g andThen (_ flatMap f))

    def **[B](s2: SGen[B]): SGen[(A,B)] =
      SGen(n => apply(n) ** s2(n))
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  object SGen {

    def listOf[A](g: Gen[A]): SGen[List[A]] = {
      SGen(n => g.listOfN(n))
    }


    def listOf1[A](g: Gen[A]): SGen[List[A]] = {
      SGen(n => g.listOfN(n max 1))
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      Gen.forAll(g(_))(f)

  }

  case class Gen[+A](sample: State[RNG,A]) {

    def map[B](f: A => B) :Gen[B] = {
      Gen(sample.map(f))
    }

    def flatMap[B](f: A => Gen[B]) :Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }


    def listOfN(n: Int): Gen[List[A]] = {
      Gen.sequence(List.fill(n)(this))
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))

    def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

    def unsized: SGen[A] = {
      SGen(n => this)
    }

    def **[B](g2:Gen[B]):Gen[(A, B)] = {
      this flatMap {a =>
        g2 map {b =>
          (a, b)
        }
      }
    }

  }


  object Gen {

    def unit[A](a: => A): Gen[A] = {
      Gen(State.unit(a))
    }

    def map2[A, B, C](a:Gen[A], b:Gen[B])(f:(A, B) => C) :Gen[C] = {
      a.flatMap{ a=>
        b.map{ b =>
          f(a, b)
        }
      }
    }

    def boolean: Gen[Boolean] = {
      Gen(State(RNG.boolean))
    }



    def sequence[A](list:List[Gen[A]]):Gen[List[A]] = {
      list.foldRight(unit(List[A]()))((ga, glist) => map2(ga, glist)(_ :: _))
    }

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(RNG.nonNegativeInt)).map(i => start + i % (stopExclusive - start))
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap(if (_) g1 else g2)
    }

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      val g1Threshold = g1._2 / g1._2 + g2._2
      Gen(State(RNG.double2)).flatMap(d => if (d > g1Threshold) g1._1 else g2._1)
    }

    val uniform: Gen[Double] = Gen(State(RNG.double))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def stringN(n: Int): Gen[String] =
      listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

    import chapter5.Ex.Stream


    def forAll[A](g:Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props:Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop:Prop = props.map(p => Prop {(max, _, rng) => p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }


    def forAll[A](as:Gen[A])(f:A => Boolean) :Prop = Prop {
      (max, n, rng) =>
        randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }.find(_.isFalsified).getOrElse(Passed)
    }



    def randomStream[A](g:Gen[A])(rng:RNG):Stream[A] = {
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    }

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


    def checkPar(p: Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)

    def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => (s => i))
  }


  type MaxSize = Int


  case class Prop(run:(MaxSize,TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = Prop {
      (maxSize, n, rng) => run(maxSize, n, rng) match  {
        case Passed | Proved => p.run(maxSize, n, rng)
        case x => x
      }
    }
    def ||(p: Prop): Prop = Prop {
      (maxSize, n, rng) => run(maxSize, n, rng) match {
        case Passed | Proved=> Passed
        case x => p.run(maxSize, n, rng)
      }
    }

    def tag(msg: String) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }



  object Prop {
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String


    sealed trait Result {
      def isFalsified:Boolean
    }

    case object Passed extends Result {
      override def isFalsified: Boolean = false
    }

    case class Falsified (failure:FailedCase, successes:SuccessCount) extends Result{
      override def isFalsified: Boolean = true
    }


    case object Proved extends Result {
      override def isFalsified: Boolean = true
    }


    def run (p:Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())):Unit = {
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
        case Passed => println(s"+ OK, passed $testCases tests.")
        case Proved => println(s"+ OK, proved property.")
      }
    }

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Passed else Falsified("()", 0)
    }

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      Gen.forAll(S ** g) { case s ** a => f(a)(s).get }
  }


  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)){ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }


  val sortedProp = forAll(listOf(smallInt)){ns =>
    val sorted = ns.sorted
    sorted.isEmpty || sorted.tail.isEmpty || !ns.zip(sorted.tail).exists{case (a, b) => a > b}
  }


  val ES = Executors.newCachedThreadPool()

  val p2 = checkPar{
    Par.equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val S = Gen.weighted( choose(1,4).map(Executors.newFixedThreadPool) -> .75, unit(Executors.newCachedThreadPool) -> .25)

  lazy val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  val forkProp = Prop.forAllPar(pint2)(i => Par.equal(Par.fork(i), i)) tag "fork"

  val takeWhileProp = forAll(listOf(smallInt)){l =>
    val f :(Int => Boolean) = _ < 5
    l.takeWhile(f) ++ l.dropWhile(f) == l
  }

  val isEven = (i: Int) => i%2 == 0

  val takeWhileProp2 = forAll(listOf(smallInt))(ns => ns.takeWhile(isEven).forall(isEven))

}
