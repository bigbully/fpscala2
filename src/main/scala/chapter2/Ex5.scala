package chapter2

/**
  * User: bigbully
  * Date: 15/12/2
  * Time: 下午11:17
  */
object Ex5 extends App{

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

}
