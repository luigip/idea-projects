package ijeuler

object Primes {
  /** Return list of primes using (unoptimized) Sieve of Eratosthenes algorithm
   * @param max Returns primes up to this value
   */
  def getList(max: Int): List[Int] = {
    val sieve = Array.fill(max + 1){true}
    sieve(0) = false
    sieve(1) = false
    var p = 2
    while (p * p <= max) {
      var i = p * 2
      while (i <= max) {
        sieve(i) = false
        i += p
      }
      p += 1
      while (p * p <= max && !sieve(p)) p += 1
    }
    val lstb = new scala.collection.mutable.ListBuffer[Int]
    for (i <- 1 to max; if sieve(i)) lstb += i
    lstb.toList
  }
  
  /**
   * A Stream of primes formed by dividing by all the lower primes
   */
  val primeStream: Stream[Int] =
    2 #:: primeStream.map { 
      i => Stream.from(i + 1).find { 
        j => primeStream.takeWhile { k => k * k <= j }.forall { j % _ > 0 }
      }.get
    }
}


















