package ijeuler

// runs in about 1.6 s on Java 64-bit
object P071 extends App {
  def hcf(a: Int, b: Int): Int = if (b == 0) a else hcf(b, a % b)  

  val results = for {
    d <- 1 to 1000000
    n = d * 3 / 7
    cf = hcf(d, n)
    n1 = n / cf
    d1 = d / cf
    if !(n1 == 3 && d1 == 7)
  } yield (3 / 7.0 - n.toDouble / d, n1)
 
  val res = results.minBy(_._1)._2
  
  println(res)
}