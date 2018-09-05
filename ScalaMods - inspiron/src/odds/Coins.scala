package odds
import Memoization._

object Coins extends App {

  def f(ms: List[Int], n: Int): Int = ms match {
    case h :: t =>
      if (h > n) 0 else if (n == h) 1 else f(ms, n - h) + f(t, n)
    case _ => 0
  }

  val g: MFunc2[List[Int], Int, Int] = memo{
    (ms, n) => ms match {
      case h :: t =>
        if (h > n) 0 else if (n == h) 1 else g(ms, n - h) + g(t, n)
      case _ => 0
    }
  }

  val r = timed {
    g(List(1, 2, 5, 10, 20, 50, 100, 200), 300)
  }

  println("result = " + r)
  // 0.922 as method
  // 2.578 as function
  // 5.281 as tupled function
  //ans: 471363

  def timed[T](f: => T) = {
    val t0 = System.currentTimeMillis()
    val r = f
    val time = System.currentTimeMillis() - t0
    println("Took %.3f".format(time/1000d) + " seconds")
    r
  }

}