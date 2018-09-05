package odds

import testingRJ.Benchmark


/**
 * Done for http://stackoverflow.com/q/8274726/770361
 * 
 * Traverses a list of Ints and finds the N largest
 */
object Demo {
  def main(args: Array[String]) {
    val c = List(1,2,3,4,5,6,5,4,3,2,1,-5,-4,10,7,10,12)
    val d = "the quick brown fox jumps over the lazy dog"
    val res = topNs(c, 3)
    
    println(res)
  }
  
  /*
   * Slower
   */
  def topN[A](xs: TraversableOnce[A], n: Int)(implicit ord: Ordering[A]) = {
    var ss = collection.SortedSet[A]()
    xs foreach { e =>
      if (ss.size < n || ord.gt(e, ss.firstKey)) ss += e
      if (ss.size > n)                           ss -= ss.firstKey
    }
    ss
  }

  /**
   * Fast
   */
  def topNs(xs: TraversableOnce[Int], n: Int) = {
    var ts = List[Int]()
    var min = Int.MaxValue
    var len = 0
    xs foreach { e =>
      if (len < n || e > min) {
        ts = (e :: ts).sorted
        min = ts.head
        len += 1
      }
      if (len > n) {
        ts = ts.tail
        min = ts.head
        len -= 1
      }    
    }
    ts
  }  
  
//  def topN[A](xs: Traversable[A], n: Int)(ss: Set[A] = Set[A](), min: A = null)
//      (implicit ord: Ordering[A]) = 
//    if (xs.isEmpty) ss 
//    else ss.size match {
//      case 0          => topN(xs.tail, n)(ss + xs.head, ss.head)
//      case x if x < n => topN(xs.tail, n)(ss + xs.head, ord.min(min, xs.head))
//      case x          => 
//        if (ord.gt(xs.head, min))
//                         topN(xs.tail, n)(ss - min + xs.head, ord.min(min, xs.head))
//    }
//    xs foreach { e =>
//      if (ss.size < n || ord.gt(e, ss.firstKey)) {
//        ss += e
//      }
//      if (ss.size > n) {
//        ss -= ss.firstKey
//      }
//    }
//    ss
//  }
  
}


object TestGen extends Benchmark {
  val rand = new java.util.Random
  val nums = (1 to 100000).map(_ => rand.nextInt).toList 
  var value = List[Int]()
  
  import Demo._
  
  def run {
    for (i <- 1 to 10) {
      value = topNs(nums, 10)
      
    }
    println(value)
  }
}

object TestRex extends Benchmark {
  val rand = new java.util.Random
  val nums = (1 to 100000).map(_ => rand.nextInt).toList 
  var value = List[Int]()
  
  def topN(s: List[Int], n: Int) = {
    val arr = s.toArray
    java.util.Arrays.sort(arr)
    arr.takeRight(n).toList
  }
  
  def run {
    for (i <- 1 to 10) {
      value = topN(nums, 10)
      
    }
    println(value)
  }
}

object TestX extends Benchmark {
  val rand = new java.util.Random
  val nums = (1 to 100000).map (_ => rand.nextInt()).toList.view
  var value = List[Int]()
  
  import X._
  
  def run {
    for (i <- 1 to 10) {
      value = elite(nums, 20)
      
    }
    println(value)
  }
}

import scala.collection.SeqView
import scala.math.min
object X {

    def  elite(s: SeqView[Int, List[Int]], k:Int):List[Int] = {
        s.sorted.reverse.force.slice(0,min(k,s.size))
    }

    def elite2(s: SeqView[Int, List[Int]], k:Int, s2:List[Int]=Nil):List[Int] = {
        if( k == 0 || s.size == 0) s2.reverse
        else {
            val m = s.max
            val parts = s.force.partition(_==m)
            val whole = if( parts._1.size > 1) parts._1.tail:::parts._2 else parts._2
            elite2( whole.view, k-1, m::s2 )
        }
    }

    def main(args:Array[String]) = {
        val N = 1000000/3
        val x = List(N to 1 by -1).flatten.map(x=>List(x,x,x)).flatten.view
        println(elite2(x,20))
        println(elite(x,20))
    }
}
