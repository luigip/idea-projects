package ijeuler

object P032 extends App {
  val nums = ('1' to '9').toSet
  val prods = for {
    a <- 1 to 10000
    b <- 1 to 10000 / a
    if (a.toString + b + (a * b)).toSet == nums
  } yield a * b
  val res = prods.distinct.sum

  println(res)
}