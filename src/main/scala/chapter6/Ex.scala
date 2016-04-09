package chapter6

/**
  * User: bigbully
  * Date: 16/2/8
  * Time: 下午8:43
  */
object Ex extends App{


  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {

    def boolean(rng: RNG): (Boolean, RNG) =
      rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

    def double(rng: RNG): (Double, RNG) = {
      val (v, rng2) = nonNegativeInt(rng)
      (v / (Int.MaxValue.toDouble + 1), rng2)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (v, rng2) = rng.nextInt
      (if (v >= 0) v else -(v + 1) , rng2)
    }

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }
    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((d, i), r2)
    }
    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count == 0) {
        (List.empty[Int], rng)
      }else {
        val (i, r1) = rng.nextInt
        val (list, r) = ints(count - 1)(r1)
        (i :: list, r)
      }
    }

    val int:Rand[Int] = _.nextInt


    def unit[A](a:A):Rand[A] = rng => (a, rng)

    def map[A, B](s:Rand[A])(f:A => B) : Rand[B] = {
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
    }

    def nonNegativeEven:Rand[Int] =
      map(nonNegativeInt)(i => i - i % 2)

    def double2:Rand[Double] =
      map(nonNegativeInt)( _ / ((Int.MaxValue.toDouble + 1)))


    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {
        val (v1, rng2) = ra(rng)
        val (v2, rng3) = rb(rng2)
        (f(v1, v2), rng3)
      }
    }


    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      rng => {
        val (finalRng, result) = fs.foldRight((rng, List.empty[A])){
          case (rd, (oldRng, list)) => {
            val (v, newRng) = rd(oldRng)
            (newRng, v::list)
          }
        }
        (result, finalRng)
      }
    }

    def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List.empty[A]))((ra, rla) => map2(ra, rla)(_ :: _))
    }


    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      rng => {
        val (v, rng2) = f(rng)
        g(v)(rng2)
      }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) {
          unit(mod)
        }else {
          nonNegativeLessThan(n)
        }
      }


    def map_[A, B](s:Rand[A])(f:A => B) : Rand[B] = {
      flatMap(s)(i => unit(f(i)))
    }


    def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra){a =>
        map(rb){b =>
          f(a, b)
        }
      }
    }


    def both[A, B](ra:Rand[A], rb:Rand[B]) :Rand[(A, B)] = {
      map2(ra, rb)((_, _))
    }

    val randIntDouble = RNG.both(int, double)
    val randDoubleInt = RNG.both(double, int)

  }





  case class State[S,+A](run: S => (A,S)) {


    import chapter6.Ex.State._

    def map[B](f: A => B): State[S, B]= {
      flatMap(i => unit(f(i)))
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State{ s =>
        val (v, s2) = run(s)
        g(v).run(s2)
      }
    }

    def map2[B, C](s:State[S, B])(f:(A, B) => C):State[S, C] = {
      flatMap{ a =>
        s.map { b=>
          f(a, b)
        }
      }
    }



  }

  object State {
    def unit[S, T](a:T):State[S, T] = State(s => (a, s))

    def get[S]:State[S, S] = State(s => (s, s))

    def set[S](s:S):State[S, Unit] = State(_ => ((), s))

    def modify[S](f : S => S) = {
      for {
        s <- get
        _ <- set(f(s))
      } yield ()
    }

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
        actions match {
          case Nil => (acc.reverse,s)
          case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
        }
      State((s: S) => go(s,sas,List()))
    }
  }

  type Rand[+A] = RNG => (A, RNG)


  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)




  def simulateMachine(inputs:List[Input]):State[Machine, (Int, Int)] = {
    import chapter6.Ex.State._

    for {
      _ <- sequence(inputs.map{ input =>
        modify((s:Machine) => {
          (input, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
            case (Turn, Machine(false, candies, coins)) => Machine(false, candies - 1, coins)
          }
        })
      })
      s <- get
    } yield (s.candies, s.coins)

  }





}



