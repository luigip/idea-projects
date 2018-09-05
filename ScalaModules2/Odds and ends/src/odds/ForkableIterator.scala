package odds

import testingRJ.Benchmark


class ForkableIterator[A] (list: List[A]) extends Iterator[A] {
    var these = list
    def hasNext: Boolean = !these.isEmpty
    def next: A = 
      if (hasNext) {
        val result = these.head; these = these.tail; result
      } else Iterator.empty.next
    def fork = new ForkableIterator(these)
}

/***************************************************************************/
object IteratorTest extends Benchmark {
  val m = (1 to 1000).map(i => (i -> i)).toMap
  var value = (0, 0)
  def run = {
    for (j <- 1 to 100000) {
      val it = new ForkableIterator(m.toList)
      while (it.hasNext) {
        val it2 = it.fork
        for (i <- 1 to 10) {value = it.next}
        for (i <- 1 to 10) {value = it2.next}
      } 
    }
  }
}

object IteratorTest2 extends Benchmark {
  val m = (1 to 1000).map(i => (i -> i)).toMap
  var value = (0, 0)
  def run = {
    for (j <- 1 to 100000) {
      var it = m.iterator
      while (it.hasNext) {
        val(it1, it2) = it.duplicate
        it = it1
        for (i <- 1 to 10) {value = it.next}
        for (i <- 1 to 10) {value = it2.next}
      } 
    }
  }
}

object IteratorTest3 extends Benchmark {
  val m = (1 to 1000).map(i => (i -> i)).toMap
  var value = (0, 0)
  def run = {
    for (j <- 1 to 100000) {
      var it = m.iterator
      while (it.hasNext) {
        val lst = it.toList
        val (it1, it2) = (lst.iterator, lst.iterator)
        it = it1
        for (i <- 1 to 10) {value = it.next}
        for (i <- 1 to 10) {value = it2.next}
      } 
    }
  }
}



object CaseTest extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: t => if (x <= y) isOrdered(l.tail) else false
  }
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}
// testing whether changing order of case clauses makes a difference
// result: not noticeable on long lists
//         on short lists, yes slightly, but opposite to way expected... why?
// probably a garbage collection issue?
object CaseTest2 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = l match {
    case x :: y :: t => if (x <= y) isOrdered(l.tail) else false
    case Nil => true
    case x :: Nil => true
  }
  
  val m = (1 to 1000).toList.reverse
  var value = false
  def run = {
    for (j <- 1 to 100000000) {
      value = isOrdered(m)
    }
  }
}

// testing if x match a::b::c => f(b::c) is slower than f(x.tail)  
// result: yes, over 3 x slower
object CaseTest3 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: t => if (x <= y) isOrdered(y::t) else false
  }
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}
// test if pattern matching on just tail rather than x :: y :: t is more performant
// result: No, same performance
object CaseTest4 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = l match {
    case Nil         => true
    case x :: Nil    => true
    case x :: t      => if (x <= t.head) isOrdered(t) 
                        else false
  }
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}

// test if moving case to end of list matches less quickly
// result: no, same
object LetterTest extends Benchmark {
  var value: Symbol = _ // dummy field to ensure method call is not optimized away 
  var x = 1
  def run = {
    for (j <- 1 to 100000000) {
      value = x match {
        case 1 => 'a   
        case 2 => 'b
        case 3 => 'c
        case 4 => 'd
        case 5 => 'e
        case 6 => 'f
       
      } 
    }
  }
}
object LetterTest2 extends Benchmark {
  var value: Symbol = _ // dummy field to ensure method call is not optimized away 
  var x = 1
  def run = {
    for (j <- 1 to 100000000) {
      value = x match {

        case 2 => 'b
        case 3 => 'c
        case 4 => 'd
        case 5 => 'e
        case 6 => 'f
        case 1 => 'a          
      } 
    }
  }
}
// test if matching on lhs is quicker
// result: no difference
object CaseTest5 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: t if x <= y => isOrdered(l.tail)
    case _ => false
  }
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}
// test 'sliding' solution
// result: it's slow, and not much faster with 'view'
object CaseTest6 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = 
    l.view.sliding(2).forall(i => i(0) <= i(1))
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}

// zip / forall 53% slower than orig
object CaseTest7 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = 
    l.view.zip(l.tail).forall(x => x._1 <= x._2)    
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}

// 35% slower than orig
object CaseTest8 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = 
    (l, l.tail).zipped.forall(_ <= _)
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}

// 10% faster than orig
object CaseTest9 extends Benchmark {
  import Ordering.Implicits._
  def isOrdered[A: Ordering](seq: Seq[A]): Boolean = {
    if (!seq.isEmpty)
      seq.tail.foldLeft(seq.head){(previous, current) => 
        if (previous > current) return false; current
      }
    true
  }  
  
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}

//15% faster
object CaseTest10 extends Benchmark {
  def isOrdered(l:List[Int]): Boolean = { 
    val it = l.iterator
    var x = it.next() 
    while (it.hasNext) {
      val y = it.next()
      if(x > y) return false
      x = y
    }
    true
  }
  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}
// 23% faster
object CaseTest11 extends Benchmark {
  def isOrdered(l: List[Int], prev: Int = Int.MinValue): Boolean =
    if(l.isEmpty) true
    else if (l.head < prev) false
    else isOrdered(l.tail, l.head)

  val m = (1 to 1000).toList
  var value = false
  def run = {
    for (j <- 1 to 100000) {
      value = isOrdered(m)
    }
  }
}
object CaseTest12 extends Benchmark {
  def isOrdered(list: List[Int]): Boolean = 
    list.sliding(2).map({ case List(a, b) => a < b }).forall(identity)
    
  val m = (1 to 1000).toList ++ (1 to 1000).toList.reverse
  var value = false
  def run = {
    for (j <- 1 to 1000) {
      value = isOrdered(m)
    }
  }
}
object CaseTest13 extends Benchmark {
  def isOrdered(list: List[Int]): Boolean =
    list.sliding(2).map(i => i(0) < i(1)).forall(identity)

  val m = (1 to 1000).toList ++ (1 to 1000).toList.reverse
  var value = false
  def run = {
    for (j <- 1 to 1000) {
      value = isOrdered(m)
    }
  }
}

object TestAll extends App {
//  CaseTest.main(Array("5"))
//  CaseTest2.main(Array("5"))
//  CaseTest7.main(Array("5"))
//  CaseTest8.main(Array("5"))
//  CaseTest9.main(Array("5"))
//  CaseTest10.main(Array("5"))
//  CaseTest11.main(Array("5"))
  CaseTest12.main(Array("5"))
  CaseTest13.main(Array("5"))            
}