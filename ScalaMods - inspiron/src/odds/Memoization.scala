package odds

import collection.mutable

object Memoization extends App {

  class MFunc[A,R](f: A => R) extends (A => R) {
    private val cache = new mutable.HashMap[A,R]()
    def apply(a: A) = cache.getOrElseUpdate(a, f(a))
    def memo = this
  }

  implicit def functionIsMFunc[A,B](f: A => B) = new MFunc(f)


  class MFunc2[A,B,R](f: (A, B) => R) extends ((A, B) => R) {
    private val cache = new mutable.HashMap[(A, B),R]()
    def apply(a: A, b: B) = cache.getOrElseUpdate((a, b), f(a, b))
    def memo = this
  }

  implicit def memo[A,B,R](f: (A, B) => R) = new MFunc2(f)

//  def memo[A,B,R](f: (A, B) => R) =
}

object MemTest extends App {
  import Memoization._
  //test
  val f = {a: String => println("calculating " + a); a.length}.memo

  f("a")
  f("bb")
  f("cc")
  f("bb")
  f("a")
  f("aaa")

}