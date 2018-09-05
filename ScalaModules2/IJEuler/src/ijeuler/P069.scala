package ijeuler
import Primes.primeStream

object P069 extends App {
  
  def primeFactors(n: Int, s: Stream[Int] = primeStream): List[Int] = {
    if (n == 1) Nil
    else if (n % s.head == 0) s.head :: primeFactors(n / s.head, s)
    else primeFactors(n, s.tail)
  }
  def nOverPhi(n: Int) = 1 / primeFactors(n).distinct.map(1 - 1d/_).product
  val res = (2 to 200000).par maxBy nOverPhi
  println(res)
}

//val primeFactorsStream: Stream[Set[Int]] = Set[Int]() #:: Set[Int]() #:: Stream.from(2).
//                                             map(primeFactors(_).toSet)
//  
//def relativePrimes(n: Int) = { 
//  val npf = primeFactorsStream(n)
//  (1 until n) filter {primeFactorsStream(_) forall {! npf.contains(_)}}
//}
//  