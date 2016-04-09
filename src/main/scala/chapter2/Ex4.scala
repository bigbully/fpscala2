package chapter2

/**
  * User: bigbully
  * Date: 15/12/2
  * Time: 下午10:46
  */
object Ex4 extends App{

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => (b => f(a, b))
  }

}
