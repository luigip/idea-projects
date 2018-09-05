package odds


object IntLength extends testingRJ.Benchmark {
  val len = 10000000
  var t: Int = _
  val randoms = Array.fill(len)(util.Random.nextInt(Int.MaxValue))
  val randoms2 = Array.fill(len)(util.Random.nextInt(100000000))
  
  def lengthBase10(x: Int) =
    if      (x >= 1000000000) 10
    else if (x >= 100000000)   9
    else if (x >= 10000000)    8
    else if (x >= 1000000)     7
    else if (x >= 100000)      6
    else if (x >= 10000)       5
    else if (x >= 1000)        4
    else if (x >= 100)         3
    else if (x >= 10)          2
    else                       1
//99
  
  def len2(x: Int) = {
    var found = false
    var n = 1
    var digits = 0
    while(! found) {
      n *= 10
      digits += 1
      if (x % n == x) found = true 
    }
    digits
  }
//680
  
  def digits(x: Int) = {
    math.log10(x).toInt + 1
  }
//357
  
  def numberLength(i : Int): Int = i.toString.length
//760

  def len(x: Int, i: Int = 1): Int = {
    if (x < 10) i
    else len(x / 10, i + 1)
  }
  //460  / 161

  def lenr(x: Int, i: Int = 10, bar: Int = 1000000000): Int = {
    if (x >= bar) i
    else lenr(x, i - 1, bar / 10)
  }
  
  def run {
    
    for(i <- randoms) {
//      t = lengthBase10(i)
//      t = digits(i)
//      t = numberLength(i)
//      t= len2(i)
      t = len(i)
//      t = lenr(i)
    }
    
  }

}