package chapter2

/**
  * User: bigbully
  * Date: 15/12/2
  * Time: 下午11:20
  */
object Ex6 extends App{

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


}
