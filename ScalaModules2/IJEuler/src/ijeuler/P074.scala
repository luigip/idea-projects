package ijeuler

/**
  * Created by Rhys on 25/04/2017.
  */
object P074 extends App {

  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x-1)
  val factorial: Map[Int, Int] = (0 to 9).map (x => (x,fact(x))).toMap

  def next(x: Int) = {
    val digits = x.toString.map(_.asDigit)
    digits.map(factorial).sum
  }

//  val test = 69
//  print(test)
//  loop(test, Set(test))

  def numbersInChain(x: Int, visited: Set[Int]): Set[Int] = {
    val n = next(x)
    //print(" -> "+n)
    if(visited(n)) {visited} else {
      numbersInChain(n, visited + n)
    }
  }

  def with60(x: Int, res: Set[Int]): Set[Int] = {
    if (x == 0) res else {
      val number = numbersInChain(x, Set(x)).size
      if (number == 60) with60(x-1, res + x) else with60(x-1, res)
    }
  }

  val res = with60(1000000, Set.empty).size

  println(res)
}
