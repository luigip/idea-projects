package ijeuler
/*
 * More complicated than it needs to be. We could use tuples instead of Layer class
 * Could also just use an imperative loop rather than Streams
 * Also isPrime is pretty inefficient
 */

object P058 extends App {
  def isPrime(n: Int) = Primes.primeStream.takeWhile(i => i * i <= n).forall(n % _ != 0)
  
  class Layer(start: Int, val sideLength: Int) {
    val corners = (1 to 4) map (start + _ * (sideLength - 1))
    lazy val primesCount = corners map isPrime count(true ==)
    override def toString = "Side: " + sideLength + " Corners: " + corners.mkString(" ")
  }
  
  object StartLayer extends Layer(0, 1) {
    override val corners = Vector(1)
    override lazy val primesCount = 0
  }
  
  val ls: Stream[Layer] = StartLayer #:: ls.map(i => new Layer(i.corners.last, i.sideLength + 2))
  
  def getSide(s: Stream[Layer], corners: Int, primes: Int): Int = {
    if (s.head.sideLength > 9 && primes / corners.toDouble < 0.1) s.head.sideLength
    else getSide(s.tail, corners + 4, primes + s.tail.head.primesCount)
  } 
  
  val res = getSide(ls, 1, 0) 
  println(res)
}

object P058b extends App {
  def isPrime(n: Int) = Stream.from(2).takeWhile(i => i * i <= n).forall(n % _ != 0)
  
  def getLayer(n: Int, ps: Double): Int = {
    if (ps / (n * 2 - 1) < 0.1) n
    else getLayer(n + 2, ps + Range.Int(n * n, (n-2)*(n-2), 1 - n).count(isPrime))
  }
  println(getLayer(5, 3))
}