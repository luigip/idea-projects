package code

class S99Int(val start: Int) {
  import S99Int._
// P31
//  def isPrime(n: Int) = ( if(n < 2) false
//    else (2 to math.sqrt(n).toInt) forall (n % _ != 0) )

  def isPrime: Boolean =
    (start > 1) && (primes takeWhile { _ <= math.sqrt(start) } forall { start % _ != 0 })

  def isCoprimeTo(n: Int) = gcd(start, n) == 1

  def totient = (1 to start).filter(_.isCoprimeTo(start)).length

  def primeFactors: List[Int] = {
    val it = primes.toIterator
    def get(n: Int, p:Int, lst: List[Int]): List[Int] = n match {
      case 1                => lst
      case i if(i % p == 0) => get(n / p, p, p :: lst)
      case _                => get(n, it.next(), lst)
    }
    get(start, it.next(), List[Int]()).reverse
  }

  def primeFactorMultiplicity = {
    primeFactors.foldRight (List[(Int, Int)]()) ((a, b) => (a, b) match {
      case (_, Nil)                   => (a, 1) :: b
      case (i, _) if (i == b.head._1) => (a, b.head._2 + 1) :: b.tail
      case _                          => (a, 1) :: b
    })
  }

  def pfm2 = primeFactors.distinct map{i => (i, primeFactors.filter(_ == i).length)}

  def phi = {
    val p = primeFactorMultiplicity
    (1 /: p)((a, b) => a * (b._1 - 1) * math.pow(b._1, (b._2 - 1)).toInt)
  }

  def goldbach:(Int, Int) = {
    if ((start > 2) && (start % 2 == 0)) {
      val p1 = primes.dropWhile( i => !(start - i).isPrime ).head
      (p1, start - p1)
    } else {
      throw new IllegalArgumentException
    }
  }

  def goldbach2: (Int,Int) =
    primes takeWhile { _ < start } find { p => (start - p).isPrime } match {
      case None     => throw new IllegalArgumentException
      case Some(p1) => (p1, start - p1)
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }

  def listPrimesinRange (r: Range) = {
    primes.dropWhile(_ < r.head).takeWhile(_ <= r.last).toList
  }
}

object Test extends App {
  import code.S99Int._

  println(28.goldbach)
}


