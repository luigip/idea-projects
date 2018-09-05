package ijeuler

object P001 {
  val res = (1 to 999).filter{ i => i % 3 == 0 || i % 5 == 0 }.sum
  println(res)
}

object P002 {
  def fibb(p: Int, q: Int, total: Int, max: Int): Int = {
    val r = p + q
    if (r > max) total
    else {
      val t = if (r % 2 == 0) total + r else total
      fibb(q, r, t, max)
    }
  }
  println(fibb(1, 2, 2, 4000000))
}

object P002_YT extends App {
  def fib(a: Int = 0, b: Int = 1): Stream[Int] = a #:: fib(b, a + b)
  println(fib().filter{_ % 2 == 0}.takeWhile{_ < 4000000}.sum)
}

object P003 {
  def lpf(n: Long, f: Long): Long = {
    if (f * f > n) n
    else if (n % f == 0) lpf(n / f, f)
    else lpf(n, f + 1)
  }
  println(lpf(600851475143L, 2))
}

object P004 {
  def isPalindrome (n: Int) = n.toString == n.toString.reverse
  val R = 100 until 1000
  val pals = {
    for (i <- R; j <- R; if isPalindrome(i * j)) yield (i * j)
  }
  println (pals max)
}

object P005 extends App {
  def isDivis(x:Int) = (1 to 20) forall {x % _ == 0}
  def find(n:Int):Int = if (isDivis(n)) n else find (n+2)

  val t = System.nanoTime;
  println (find (2))
  println(System.nanoTime - t)
}

object P005_V2 { // Using tail recursion: forall is very slow
  def isDivis(x:Int, i:Int):Boolean = {
    if(i > 20) true
    else if(x % i != 0) false
    else isDivis(x, i+1)
  }
  def find(n:Int):Int = if (isDivis(n, 2)) n else find (n+2)

  val t = System.nanoTime;
  println (find (2))
  println(System.nanoTime - t)
}

object P006 extends App {
  val MAX = 100
  val sumsq = ((1 to MAX) map {x => x * x}).sum
  val s = (1 to MAX).sum
  println(s * s - sumsq)
}

object P007 extends App {
  val REQUIRED_NUMBER = 10001
  val MAXIMUM_PRIME = 1000000
  println(Primes.getList(MAXIMUM_PRIME)(REQUIRED_NUMBER - 1))
}

object P008 extends App {
  val raw = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"""

  val num = raw.replace(String.format("%n"), "")

  def getmax(s:String, max:Int):Int = {
    if (s.length < 5) max
    else {
      val p = s.substring(0, 5).map{x => Character.getNumericValue(x)}.product
      if (p > max) getmax(s.tail, p)
      else  getmax(s.tail, max)
    }
  }

  println (getmax (num, 0))
}

object P009 {
  def find =
    for {
      a <- (1 to 500)
      b <- (1 to 500)
      c = 1000 - a - b
      if (a * a + b * b == c * c)
    } yield (a * b * c)

  println(find(0))
}