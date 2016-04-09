package chapter10

import chapter3.Ex.Tree
import chapter7.ExNonBlocking.Par
import chapter8.Ex.{Prop, Gen}

/**
  * User: bigbully
  * Date: 16/3/27
  * Time: 下午2:06
  */
object Ex extends App{

  trait Monoid[A] {
    def op (a1:A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition:Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication:Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr:Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd:Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]:Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = a => a
  }

  def monoidLaws1[A](m:Monoid[A], gen:Gen[A]) :Prop = {
    Gen.forAll(gen){
      a => m.op(a, m.zero) == a
    }
  }

  def monoidLaws1[A](m:Monoid[A], gen:Gen[A]) :Prop = {
    Gen.forAll{
      for {
        a <- gen
        b <- gen
        c <- gen
      } yield (a, b, c)
    } {
      case (a, b, c) => m.op(a, m.zero) == a && m.op(m.zero, a) == a &&
        m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }
  }

  val words = List("Hic", "Est", "Index")

  words.foldLeft(stringMonoid.zero)(stringMonoid.op)


  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldLeft[A, B](as:List[A], z:B, f:(B, A) => B):B ={
    foldMap(as, endoMonoid[B])(a => b => f.curried(b)(a))(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }


  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size == 0){
      m.zero
    }else if (v.size == 1) {
      f(v(0))
    }else {
      val (v1, v2) = v.splitAt(v.size / 2)
      m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
    }
  }


  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)((aa1, aa2) => m.op(aa1, aa2))

    override def zero: Par[A] = Par.unit(m.zero)
  }


  import chapter7.ExNonBlocking.Par._

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    Par.parMap(v)(f).flatMap{list =>
      foldMapV(list, par(m))(b => Par.lazyUnit(b))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= y2))
          case (x, None) => x
          case (None, x) => x
        }
      }

      override def zero: Option[(Int, Int, Boolean)] =None
    }

    foldMapV(ints, mon)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }


  sealed trait WC
  case class Stub(chars:String) extends WC
  case class Part(lstub:String, words:Int, rStubS:String) extends WC

  val wcMonoid:Monoid[WC] = new Monoid[WC] {

    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(ls, words, rs)) => Part(c + ls, words, rs)
        case (Part(ls, words, rs), Stub(c)) => Part(ls, words, rs + c)
        case (Part(ls1, words1, rs1), Part(ls2, words2, rs2)) => Part(ls1, (if ((rs1 + ls2).isEmpty) 0  else 1), rs2)
      }
    }

    override def zero: WC = Stub("")
  }


  def wordCount(word:String) :Int = {
    def wc(c:Char):WC = {
      if (c.isWhitespace) {
        Part("", 0, "")
      }else {
        Stub(c.toString)
      }
    }

    def unStub(s:String) = s.length min 1

    val result: WC = foldMapV(word.toIndexedSeq, wcMonoid)(wc)
    result match {
      case Stub(s) => unStub(s)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }
  }

  trait Foldable[F[_]] {

    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = {
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    }
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = {
      foldMap(as)(a => (b:B) => f(b, a))(dual(endoMonoid[B]))(z)
    }

    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    }
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = {
      foldRight(fa)(List[A]())(_ :: _)
    }
  }


  sealed trait Tree[+A]
  case class Leaf[A](value:A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {


    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
      as match {
        case Leaf(v) => f(v)
        case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
    }

    override def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = {
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Leaf(v) => f(z, v)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
        as match {
          case Some(v) => f(v, z)
          case None => z
        }
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Some(v) => f(z, v)
        case None => z
      }
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case Some(v) => f(v)
        case None => mb.zero
      }
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = {
      (a1.keySet ++ a2.keySet).foldLeft(zero) {
        (z, k) => z.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }
    }

    override def zero: Map[K, V] = Map[K, V]()
  }

  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))


  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    override def zero: (A) => B = a => B.zero
  }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {

    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a:A) => Map(a -> 1))
  }


}
