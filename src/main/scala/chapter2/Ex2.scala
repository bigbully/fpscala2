package chapter2

import scala.annotation.tailrec

/**
  * User: bigbully
  * Date: 15/12/2
  * Time: 上午8:17
  */
object Ex2 extends App{

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {

    @tailrec
    def go(index:Int) :Boolean= {
      if (index == as.length -1 ) {
        true
      } else {
          if (gt(as(index+1), as(index))) {
            go(index + 1)
          }else {
            false
          }
      }
    }

    go(0)
  }

  println(isSorted[Int](Array(1,2,8,4,5,6), _ > _ ))


}
