package ijeuler


object P072 {
  def hcf(a: Int, b: Int): Int = if (b == 0) a else hcf(b, a % b)  

  val results = for {
    d <- 1 to 8
    n = d * 3 / 7
    cf = hcf(d, n)
    n1 = n / cf
    d1 = d / cf
    if !(n1 == 3 && d1 == 7)
  } yield (3 / 7.0 - n.toDouble / d, n1)
  
List(1,2,3).iterator.hasNext
  val res = null
  println(res)  
}