package odds


object Utils {

  /*
    Significant figures: string representation
  */
  def sf(d: Double, places: Int) = 
    BigDecimal(d).round(new java.math.MathContext(places)).toString()

  class Stat(xs: Int*) {
    val n = xs.size
    val m = xs.sum.toDouble / n
    val ss = xs.map{i => math.pow(m - i, 2) }.sum
    val pv = ss / n
    val sv = ss / (n - 1)
    val psd = math.sqrt(pv)
    val ssd = math.sqrt(sv)
    override def toString = ("Stat(N = %d, Mean = %.1f, Sum of Squares = %.1f, Population Variance = %.1f, " +
      "Population Standard Deviation = %.1f, Sample Variance = %.1f, Sample Standard Deviation = %.1f)"
      ).format(n, m, ss, pv, psd, sv, ssd)
  }


  def main(args: Array[String]) {
    println(new Stat(22,13,18,16))
  }
}