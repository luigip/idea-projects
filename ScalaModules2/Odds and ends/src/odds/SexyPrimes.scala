package odds

  object SexyPrimes1 extends  App {
  def isPrime(n:Int):Boolean =
    (2 until n) forall {n%_!=0}
    //4906
    
  def isPrime2(n: Int) = {
    var res = true
    var i = 2
    while (i < n && res) {
      res = n % i != 0
      i += 1
    }
    res
  }
    
  def isPrime3(n: Int, i: Int = 2): Boolean = 
    if (i == n) true 
    else if (n % i != 0) isPrime3(n, i + 1)
    else false
  //2182

  def isPrime4(n: Int, i: Int = 2): Boolean =
    i == n || n % i != 0 && isPrime4(n, i + 1)
    
  
    
  def sexyPrimes(n:Int) = {
    (11 to n) map (i => (i - 6, i)) filter { case (a, b) =>
      isPrime3(a) && isPrime3(b)
    }
  }
  def sexyPrimes2(n: Int, i: Int = 11, out: Seq[(Int, Int)] = Vector.empty): Seq[(Int, Int)] = {
    if (i == n) out
    else if (isPrime3(i - 6) && isPrime3(i)) sexyPrimes2(n, i+1, out :+ (i-6, i))
    else sexyPrimes2(n, i+1, out)
  }
  def sexyPrimes3(n:Int) = {
    (11 to n)collect { case i if isPrime3(i-6) && isPrime(i) => (i-6, i) }
    //4493
  }

  def sexyPrimes4(n:Int) = {
    (11 to n) withFilter { i =>
      isPrime(i - 6) && isPrime4(i)
    } map (i => (i - 6, i))
    // 2116
  }
      // with isPrime2: 2189
      // with isPrime: 5712


//      (11 to n) map { i => List(i-6, i) } filter { i =>
//        i.forall(isPrime(_))
       // with isPrime2: 2296
      // with isPrime: 5957
      
    
    
  

  val a = System.currentTimeMillis()
    val res = sexyPrimes4(100*1000)
  println(res)
  val b = System.currentTimeMillis()
  println((b-a).toString + " mils")
  println("length: " + res.length)
// as on SO, takes 5928 ms
  // with Benchmark 10k: 120 ms
  // without 10k: 294 ms
  // 30k: 929
    // with isPrime2 30k: 402
}


object SexyPrimes2 extends  testingRJ.Benchmark {

  def isPrime(n:Int):Boolean =
    (2 until n) forall {n%_!=0}

  def sexyPrimes(n:Int) = {
    (11 to n) map { i => List(i-6, i) } filter { i =>
      i.forall(isPrime(_))
    }
  }

    def run {
      println(sexyPrimes(30*1000))  // more like 8-9 seconds      
    }

  // as on SO, takes 5928 ms
  // with Benchmark 10k: 120 ms
  // 30k: 900
}