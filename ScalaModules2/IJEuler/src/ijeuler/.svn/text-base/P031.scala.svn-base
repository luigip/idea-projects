package ijeuler

object P031_iterative extends App {
// adapted from a C solution on PE forums... still don't quite understand it!
  def findAll(money: Int, coins: List[Int]): Int = {
    var sum = 0
      coins foreach {i =>
        if (money - i == 0) sum += 1
        else if (money - i > 0)
          sum += findAll (money - i, coins dropWhile (_ > i))
      }
    sum
  }
  val res = findAll(200, List(200, 100, 50, 20, 10, 5, 2, 1))
  println(res)
}

object P031_Pavel extends App {
  var count:Long = 0
  def find(ms: List[Int], n: Int): Int = {count +=1;ms match {
    case h :: t =>
      if (h > n) 0
      else if (n == h) 1
      else find(ms, n - h) + find(t, n)
    case _ => 0
  }}
  val res = find(List(1, 2, 5, 10, 20, 50, 100, 200), 200)

  println(res)
  println(count)
}

object P031_Daniel extends App {
  class IterationForCoin(stream: Stream[Int], coin: Int) {
    val (lower, higher) = stream splitAt coin
    val next: Stream[Int] = lower #::: ((higher zip next) map {case (a, b) => a + b})
  }
  val coins = List(1, 2, 5, 10, 20, 50, 100, 200)
  val result = coins.foldLeft(1 #:: Stream.fill(1000)(0)) {
    (stream, coin) => new IterationForCoin(stream, coin).next
  }.last
  println(result)
}

object P031_dynamic extends App {
  val TOTAL = 1000
  val coins = Array(1, 2, 5, 10, 20, 50, 100, 200)
  val ways = new Array[Int](TOTAL + 1)
  ways(0) = 1
  for (coin <- coins) {
    var j = coin
    while (j <= TOTAL) {
        ways(j) += ways(j - coin)
        j += 1
    }
  }
  println("Result: " + ways(TOTAL))
}









