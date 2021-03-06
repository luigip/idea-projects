package odds

import testingRJ.Benchmark


object groupedTest extends App {
//  def bench(id: String)(block: => Unit) {
//    val times = (new testingRJ.Benchmark { def run() = block }).runBenchmark(10)
//    println(id + " " + times + " sum: " + times.sum)
//  }
  
  def splitDrop[A](xs: List[A], n: Int): List[List[A]] = {
    if (xs.size <= n) xs :: Nil
    else (xs take n) :: splitDrop(xs drop n, n)
  }
  
  def splitDropTail[A](xs: List[A], n: Int, acc: List[List[A]] = Nil): List[List[A]] = {
    if (xs.size <= n) (xs :: acc).reverse
    else splitDropTail(xs drop n, n, (xs take n) :: acc)
  }
  
  def splitSplitAt[A](xs: List[A], n: Int): List[List[A]] = {
    if (xs.size <= n) xs :: Nil
    else {
      val (h, t) = xs splitAt n
      h :: splitSplitAt(t, n)
    }
  }
  
  def grouped[A](xs: List[A], n: Int): List[List[A]] = {
    xs.grouped(n).toList
  }
  
  def runTest {
    var v = List[List[Any]]()
    var v2 = List[List[Any]]()
    var i = 0
    var t0 = System.currentTimeMillis
    while(i < 1000) {
      v = splitDropTail(lst, n)
      i += 1
    }
    v2 = v
    var t1 = System.currentTimeMillis
    println(t1 - t0)
    
    i = 0
    t0 = System.currentTimeMillis
    while(i < 1000) {
      v = splitSplitAt(lst, n).iterator.toList
      i += 1
    }
    v2 = v
    t1 = System.currentTimeMillis
    println(t1 - t0)
   
    i = 0
    t0 = System.currentTimeMillis
    while(i < 1000) {
      v = splitDrop(lst, n)
      i += 1
    }
    v2 = v
    t1 = System.currentTimeMillis
    println(t1 - t0)
    
    i = 0
    t0 = System.currentTimeMillis
    while(i < 1000) {
      v = splitSplitAt(lst, n)
      i += 1
    }
    v2 = v
    t1 = System.currentTimeMillis
    println(t1 - t0)    
    
    i = 0
    t0 = System.currentTimeMillis
    while(i < 1000) {
      v = grouped(lst, n)
      i += 1
    }
    v2 = v
    t1 = System.currentTimeMillis
    println(t1 - t0)
    
  }
//  val lst = List(1,2,3,4,5,6, "seven")
  val lst = List.range(1, 200)
  val n = 2
  runTest
//  println("split using drop   ") + runTest(splitDrop, lst, n) 
//  println("split using splitAt") + runTest(splitSplitAt, lst, n)
//  println("grouped            ") + runTest(grouped, lst, n) 
  
}