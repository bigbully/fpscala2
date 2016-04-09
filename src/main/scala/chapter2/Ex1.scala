package chapter2

/**
  * User: bigbully
  * Date: 15/11/27
  * Time: 下午10:36
  */
object Ex1 extends App{

  def fib(n: Int): Int = {
    def go(last2:Int, last1:Int, remain:Int):Int = {
      if (remain == 0) last2
      else go(last1, last1 + last2, remain - 1)
    }
    go(0, 1, n)
  }


  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))



}
