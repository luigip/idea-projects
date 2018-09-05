package ijeuler

// change again lappy

object Benchmark extends App{

  def fastDigitSum(b: BigInt): Int = {
    val bits = b.bitLength
    if (bits < 63) math.abs(b.toLong).toString.map(_-'0').sum
    else {
      val many = 256
      val zeros = math.ceil(bits*0.150515).toInt // constant is 0.5*log(2)/log(10)
      val root = (
        if (zeros<many) BigInt("1" + "0"*zeros)
        else {
          Iterator.iterate((BigInt("1"+"0"*many),many))(x => (x._1 * x._1, 2*x._2)).
            find(_._2 > zeros/2).get._1
        }
      )
      val (q,r) = b /% root
      fastDigitSum(q) + fastDigitSum(r)
    }
  }

  def stringSum(b: BigInt) = b.toString.map(_.getNumericValue).sum
  def stringSum2(b: BigInt) = b.toString.map(_.toString.toInt).sum
  def stringSum3(b: BigInt) = b.toString.map(_ - '0').sum
  def stringSum4(b: BigInt) = b.toString.map(_.asDigit).sum

  def iter (b: BigInt) = Iterator.iterate(b)(_/10).takeWhile(_>0).map(_%10).sum.toInt

  def division(b: BigInt) = sumDig(b, 0)
  def sumDig(b: BigInt, sum: Int):Int = {
    if (b == 0) sum
    else sumDig(b / 10, sum + (b % 10).toInt)
  }

  def bigSum(b: BigInt) = DigitSum.bigSum(b)

  def testMethod(f: BigInt => Int):Double = {
    val b = BigInt("12345678901234567890123456789012345678901234567890")
    val b2 = BigInt("1234567890")
    val b3 = BigInt("1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
    val b4 = BigInt(10) pow 100000
    val t0 = System.nanoTime()
    var i = 0
    while(i < 10000){
      f(b3)
      i += 1
    }
    (System.nanoTime().toDouble - t0)/1e9
  }

  def runTest(){
    var i = 0
    while (i < 5) {
      println("fastDigitSum: " + testMethod {fastDigitSum} + " seconds")
      println("stringSum:    " + testMethod {stringSum} + " seconds")
      println("stringSum2:   " + testMethod {stringSum2} + " seconds")
      println("stringSum3:   " + testMethod {stringSum3} + " seconds")
      println("stringSum4:   " + testMethod {stringSum4} + " seconds")
      println("division:     " + testMethod {division} + " seconds")
      println("iterator:     " + testMethod {iter} + " seconds")
      println("bigSum:       " + testMethod {bigSum} + " seconds")
      println()
      i += 1
    }
  }

  runTest()
}

object DigitSum {
  val memend = BigInt(10000000000000000L) :: BigInt(100000000) :: Nil

  def longSum(l: Long, sum: Int = 0): Int = {
    if (l==0) sum else longSum(l/10, sum + (l%10).toInt)
  }

  def bigSum(b: BigInt, memo: List[BigInt] = Nil): Int = {
    val bits = b.bitLength
    if (bits < 64) longSum(b.toLong)
    else {
      val mem = (
        if (memo.isEmpty) {
          val it = Iterator.iterate(memend.head)(x=>x*x).drop(1)
          var xs = memend
          while (xs.head.bitLength*4 <= bits) xs = it.next :: xs
          xs
        }
        else memo.dropWhile(_.bitLength > bits/2)
      )
      val (q,r) = b /% mem.head
      bigSum(q,memo) + bigSum(r,memo)
    }
  }
}































