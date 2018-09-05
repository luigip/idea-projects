package euler2015

/**
 * Created by Rhys on 25/03/2015.
 */


/**If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.*/

object P001 extends App {
  def nums = (1 until 1000) filter (x => x % 3 == 0 || x % 5 == 0)
  def res = nums.sum
  println(res)
}

object P002 extends App {
  val fibs: Stream[Int] = Stream(0,1) #::: fibs.zip(fibs.tail).map{case (x,y) => x + y}
  val fibsEuler = fibs drop 2
  def res = fibsEuler.takeWhile(_ < 4e6).filter(_%2 == 0).sum
  println(res)
}