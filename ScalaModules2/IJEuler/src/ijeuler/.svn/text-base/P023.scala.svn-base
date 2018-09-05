package ijeuler

import java.lang.Boolean

object P023 extends App {
  def isAbundant(n: Int) = {
    val sqrt = math.sqrt(n).toInt
    val r = (1 to sqrt).filter(n % _ == 0).toList
    (r.map(i => n / i).tail ::: r).toSet.sum > n
  }

  def sumOf2ab(n: Int) = {
    def rec(p: Int, i:Int):Boolean = {
      if(i >= p) false
      else if (abNumSet.contains(i) && abNumSet.contains(p - i)) true
      else rec(p, i + 1)
    }
    rec(n, 1)
  }
  val abNumSet = (1 to 28123).filter(isAbundant).toSet
  val result = (1 to 28123).filter(!sumOf2ab(_)).sum

  println(result)
}

object P024 extends App {
//  val result = List.range(0, 10).permutations.take(1000000).toList.last.mkString.toLong
//  println(result)
//
  def ps(s: String): Seq[String] = {
    if(s.size == 1) Seq(s)
    else s.flatMap {
      c => ps {
        s.filterNot(_ == c)
      }.map(c +)
    }
  }
  val r = ps("0123456789")(999999).toLong
  println(r)
}

object P027 extends App {
  // nb: toSet or it runs slooooow
  val plist = Primes.getList(1000000).toSet

  val lens = for {
    a <- (-999 to 999)
    b <- (-999 to 999)
  } yield (a, b, seqlen(a, b))

  def seqlen(a: Int, b:Int): Int = {
    def comp(n: Int):Int = n * n + a * n + b

    Iterator.from(0).takeWhile(plist contains comp(_)).length
  }

//  val ord = Ordering[Int].on[(Int, Int, Int)](_._3)
//  val res = lens.max(ord)
// Trying to sort the list runs out of memory... so do as above, or, slightly faster:
  val res = lens.reduceLeft((a,b) => if(a._3 > b._3) a else b)
  println("a = " + res._1 + " b = " + res._2 + " has run of " + res._3)
  println("a * b = " + res._1 * res._2)
}

object P028 extends App {
  val SIZE = 1001
  val res = 3.to(SIZE, 2).foldLeft(1)((a,b) => a + (b - 2)*(b - 2) * 4 + (b - 1) * 10)
  println(res)
}

object P029 extends App {

  val combos = for {
    a <- 2 to 100
    b <- 2 to 100
  } yield BigInt(a).pow(b)
  val res = combos.distinct.length

  println(res)
}
/*
The "view" in isSum roughly halves the execution time
 */
object P030 extends App {
  val P = 5
    // calculate highest limit we need to test to
  def maxSum(n:Int) = (math.pow(9, P) * n).toInt
  val max = maxSum( Iterator.from(1).dropWhile{i => maxSum(i) > math.pow(10,i)}.next() )
    // cache Pth powers of 0 to 9
  val powP = Array.range(0, 10) map {math.pow(_,P).toInt}
  def isSum(n: Int) = n.toString.view.map{_.asDigit}.map{powP(_)}.sum == n
  val res = (2 to max).filter{isSum}.sum

  println(res)
}
















