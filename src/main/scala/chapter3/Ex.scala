package chapter3

import scala.annotation.tailrec

/**
  * User: bigbully
  * Date: 15/12/13
  * Time: 上午11:11
  */
object Ex extends App{


  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]


  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree:Tree[A]) :Int = {
      tree match {
        case Branch(l, r) => 1 + size(l) + size(r)
        case Leaf(_) => 1
      }
    }

    def maximum(tree:Tree[Int]):Int = {
      tree match {
        case Leaf(value) => value
        case Branch(l, r) => maximum(l) max maximum(r)
      }
    }

    def depth[A](tree:Tree[A]) :Int = {
      tree match {
        case Branch(l, r) => 1 + (depth(l) max depth(r))
        case Leaf(_) => 0
      }
    }

    def map[A, B](tree:Tree[A])(f:A => B):Tree[B] = {
      tree match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
    }

    def fold[A, B](tree:Tree[A])(g:A => B)(f:(B, B) => B) :B = {
      tree match {
        case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
        case Leaf(v) => g(v)
      }
    }

    def size2[A](tree:Tree[A]) :Int = {
      fold(tree)(a => 1)(1 + _ + _)
    }


    def maximum2(tree:Tree[Int]) :Int = {
      fold(tree)(a => a)(_ max _)
    }

    def depth2[A](tree:Tree[A]) :Int = {
      fold(tree)(a => 0)((a, b) => 1 +(a max b))
    }

    def map2[A, B](tree:Tree[A])(f:A => B):Tree[B] = {
      fold(tree)(a => Leaf(f(a)):Tree[B])(Branch(_, _))
    }
  }

  println(Tree.depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))


  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def tail[A](xs:List[A]):List[A] = {
      xs match {
        case Nil => sys.error("tail of empty list")
        case Cons(_, t) => t
      }
    }

    def drop[A](xs:List[A], n:Int) :List[A] = {
      if (n == 0) {
        xs
      }else {
        xs match {
          case Nil => sys.error("drop of empty list")
          case Cons(_, t) => drop(t, n - 1)
        }
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h,t) if f(h) => dropWhile(t, f)
        case _ => l
      }

    def setHead[A](l:List[A], e:A) :List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, t) => Cons(e, t)
      }
    }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))

      }
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(l: List[Int]) =
      foldRight(l, 0.0)(_ + _)

    def product2(l: List[Double]) =
      foldRight(l, 1.0){
        case(a, _) if (a == 0.0)=> 0.0
        case(a, z) => z * a
      }

    def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

    @tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
      l match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }


    def sum3(l:List[Int]): Int = {
      foldLeft(l, 0)((b, a) => b + a)
    }

    def product3(l:List[Double]):Double = {
      foldLeft(l, 1.0){
        case (b, 0.0) => 0.0
        case (b, a) => b * a
      }
    }


    def reverse[A](l:List[A]) :List[A] = {
      foldLeft(l, Nil:List[A]){
        case (result, a) => Cons(a, result)
      }
    }

    def foldLeftByFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
      foldRight(l, (b:B) => b) {
        case (a, g) => {
          b => g(f(b, a))
        }
      }(z)
    }

    def foldRightByFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(l, (b:B) => b)((g, a) => b => g(f(a, b)))(z)
    }

    def append[A](l:List[A], r:List[A]):List[A] = {
      foldLeft(reverse(l), r)((b, a) => Cons(a, b))
    }

    def append2[A](l:List[A], r:List[A]):List[A] = {
      foldRight(l, r)(Cons(_, _))
    }

    def concat[A](l:List[List[A]]) :List[A] = {
      foldLeft(l, Nil:List[A])(append)
    }

    def add1(l:List[Int]):List[Int] = {
      foldRight(l, Nil:List[Int])((a, z) => Cons(a+1, z))
    }

    def doubleToString(l:List[Double]):List[String] = {
      foldRight(l, Nil:List[String])((a, z) => Cons(a.toString, z))
    }

    def map[A,B](l: List[A])(f: A => B): List[B] = {
      foldRight(l, Nil:List[B])((a, z) => Cons(f(a), z))
    }

    def filter[A](l:List[A])(f: A => Boolean) :List[A] = {
      foldRight(l, Nil:List[A])((a, z) => if (f(a)) Cons(a, z) else z)
    }

    def flatMap[A, B](l:List[A])(f : A => List[B]) :List[B] = {
      concat(map(l)(f))
    }

    def filterUseFlatMap[A, B](l:List[A])(f : A => Boolean) :List[A] = {
      flatMap(l)(i => if (f(i)) List(i) else Nil)
    }

    def addPairwise(l1:List[Int], l2:List[Int]) :List[Int] = {
      (l1, l2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
        case (Nil, _) => Nil
        case (_, Nil) => Nil
      }
    }

    def zipWith[A](l1:List[A], l2:List[A])(add:(A, A)=> A):List[A] = {
      (l1, l2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(add(h1, h2), zipWith(t1, t2)(add))
        case (Nil, _) => Nil
        case (_, Nil) => Nil
      }
    }


    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
      case (_,Nil) => true
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }
    @annotation.tailrec
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h,t) if startsWith(l, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
    }

    def hasSubsequence2[A](l: List[A], sub: List[A]):Boolean = {
      (l, sub) match {
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubsequence2(t1, t2)
        case (Cons(h1, t1), l2@Cons(h2, t2)) if h1 != h2 => hasSubsequence2(t1, l2)
        case (Nil, Cons(h2, t2)) => false
        case (Cons(h1, t1), Nil) => true
      }
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3)
    val total = sum(example)
  }


  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }


//  println(List.append2(List(1,2,3,4,6), List(7)))

//  println(List.hasSubsequence(List(1,2,3, 4, 5), List(2, 3, 6, 4)))
}
