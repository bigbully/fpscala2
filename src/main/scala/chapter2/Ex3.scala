package chapter2

/**
  * User: bigbully
  * Date: 15/12/2
  * Time: 下午10:30
  */
object Ex3 extends App{

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    f(a, _)
  }


  def partial2[A,B,C](a: A, f: (A,B) => C): B => C = {
    b => f(a, b)
  }
}
