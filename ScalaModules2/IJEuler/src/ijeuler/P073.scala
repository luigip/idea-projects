package ijeuler

object P073 extends App {
  
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  object Fraction {
    implicit val ordFraction = new Ordering[Fraction] {
      def compare(a: Fraction, b: Fraction) = (a - b).signum
    }
  }
  
  class Fraction(a: Int, b: Int) {
    override def toString = "(%s/%s)" format (num, den)
    override def hashCode = num * den
    override def equals(f: Any) = f match {
      case x: Fraction => implicitly[Ordering[Fraction]].compare(this, x) == 0
      case _ => false
    }
    
    private val hcf = gcd(a, b)
    
    val num = a / hcf
    val den = b / hcf
     
    def -(x: Fraction) = new Fraction(num*x.den - x.num*den, x.den * den)
    
    val signum = 
      if (num == 0) 0
      else if (num > 0 && den > 0 || num < 0 && den < 0) 1
      else -1
  }
  

  
  def howMany(dmax: Int) = {
    val hm = collection.mutable.HashSet[Fraction]()
    var i = 1
    while (i <= dmax) {
      val lower = i / 3 + 1
      val upper = (i + 1) / 2 - 1 
//      println("i = %s upper = %s lower = %s" format (i, upper, lower))
      var j = lower
      while (j <= upper) {
        hm += new Fraction (j, i)
//        println("added %s/%s" format (j, i))
        j += 1
      } 
      i += 1
    }
//    println(collection.SortedSet[Fraction]() ++ hm)
    hm.size
  }
  
  val res = howMany(12000)
  
  println(res)
  
}