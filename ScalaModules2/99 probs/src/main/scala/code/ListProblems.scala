package code

object ListProblems {
// P01
  def last[T](lst: List[T]): T = {
    if (lst.tail == Nil) lst.head
    else last(lst.tail)
  }
  //or
  def last2[T](lst: List[T]): T = lst match {
    case h :: Nil  => h
    case _ :: tail => last2(lst.tail)
    case _         => throw new NoSuchElementException
  }

// P02
  def penultimate[T](l: List[T]): T = l match {
    case h :: _ :: Nil => h
    case h :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

// P03
  def nth[T](n: Int, lst: List[T]):T = (n,lst) match {
    case (0, h :: _) => h
    case (i, _ :: t) => nth(i - 1, t)
    case (_, Nil   ) => throw new NoSuchElementException
  }

// P04
  def length[T](lst:List[T]):Int = {
    def f[T](lst:List[T], i:Int):Int = lst match {
      case _ :: t   => f(t, i + 1)
      case Nil      => i
    }
    f(lst, 0)
  }

// P05
  def reverse[T](lst: List[T]): List[T] = {
    def rev[T](old: List[T], newl: List[T]): List[T] = old match {
      case Nil => newl
      case _   => rev(old.tail, old.head :: newl)
    }
    rev(lst, Nil)
  }

// P06
  def isPalindrome[T](lst: List[T]): Boolean = lst == lst.reverse

// P07
  def flatten[T](lst: List[Any]): List[Any] = lst flatMap {
    case h: List[_] => flatten(h)
    case g => List(g)
  }

// P08
  def compress[T](lst: List[T]): List[T] =
    lst.foldRight (List[T]()) {(a, b) =>
      if (b.isEmpty || b.head != a) a :: b
      else b
    }

// P09
  def pack[T](lst: List[T]): List[List[T]] = {
    if (lst == Nil) Nil
    else lst.takeWhile(_ == lst.head) :: pack (lst.tail.dropWhile(_ == lst.head))
  }

// P10
  def encode[T](lst: List[T]): List[(Int, T)] = {
    pack(lst).map(i => (i.length, i.head))
  }

// P11
  def encodeModified[T](lst: List[T]): List[Any] = {
    pack(lst).map{ i =>
      if (i.length == 1) i(0)
    else (i.length, i.head)
    }
  }

// P12
  def decode[T](lst: List[(Int, T)]): List[T] = {
    lst.flatMap(i => List.fill(i._1)(i._2))
  }

// P13
  // lst.isEmpty instead of == Nil
  // could also use ls.span to split into 2 lists
  def encodeDirect[T](lst: List[T]): List[(Int, T)] = {
    if (lst == Nil) Nil
    else {
      val a = lst.takeWhile(_ == lst.head)
      (a.length, a(0)) :: encodeDirect (lst.tail.dropWhile(_ == lst.head))
    }
  }

// P14
  def duplicate[A](lst: List[A]): List[A] = {
   lst flatMap (i => List(i,i))
  }
  // alternatively (note . after lst and List() rather than Nil)
  // lst.foldRight(List[A]())((a,b) => a :: a :: b)

// P15
  // better to use List.fill
  def duplicateN[A](n: Int, lst: List[A]): List[A] = {
    def rep(m: Int, elem: A) = for (i <- 1 to m) yield elem
    lst flatMap (i => rep(n, i))
  }

// P16
  def drop[A](n: Int, lst: List[A]): List[A] = {
    lst.zipWithIndex.filter(i => (i._2 + 1) % n != 0).map(a => a._1)
  }

// P17
  def split [A](n: Int, lst: List[A]): (List[A], List[A]) = {
    lst splitAt n
    // or (lst take n, lst drop n)
  }

// P18
  def slice[A](n: Int, m: Int, lst: List[A]): List[A] = {
    lst.drop(n).take(m - n)
  }

// P19
  def rotate[A](n: Int, lst: List[A]): List[A] =  {
    val len = lst.length
    val m = (n % len + len) % len
    (lst drop m) ::: (lst take m)
  }

// P20
  def removeAt[A](n: Int, lst: List[A]): (List[A], A) =  {
    if (lst.isEmpty || n >= lst.length || n  < 0) throw new NoSuchElementException
    else ((lst take n) ::: (lst drop (n + 1)), lst(n))
  }

// P21
  def insertAt[A](e: A, n: Int, lst: List[A]): List[A] = {
    if (lst.isEmpty || n > lst.length || n  < 0) throw new NoSuchElementException
    else (lst take n) ::: e :: (lst drop n)
  }

// P23
  def randomSelect[A](n: Int, lst: List[A]): List[A] =  {
    def get(k: Int, res: List[A], rem: List[A]): List[A] = k match {
      case 0            => res
      case i if (i < 0) => throw new NoSuchElementException
      case i            => {
        val r = removeAt(math.random * rem.length toInt, rem)
        get(k - 1, r._2 :: res, r._1)
      }
    }
    get(n, List[A](), lst)
  }

// P24
  def lotto(n: Int, m: Int): List[Int] = {
    randomSelect(n, List.range(1, m + 1))
  }

// P25
  def randomPermute[A](lst: List[A]): List[A] = {
    randomSelect(lst.length, lst)
  }

// P26
  def combinations[A](n: Int, lst: List[A]): List[List[A]] = n match {
    case 1 => lst.map(List(_))
    case _ => lst.flatMap(i => combinations (n - 1, lst.dropWhile(_ != i).tail) map (i :: _))
  }

// P27
  //a
  def group3[A](lst: List[A]): List[List[List[A]]] = 
    for {
      c2 <- combinations(2, lst)
      c3 <- combinations(3, lst diff c2)
      c4 <- combinations(4, lst diff c2 diff c3)
    } yield List(c2, c3, c4)
    
  //b
  def group[A](sizes: List[Int], lst: List[A]): List[List[List[A]]] = sizes match {
    case Nil => List(Nil)
    case x :: xs => for {
      c <- combinations(x, lst)
      r <- group(xs, lst diff c)
    } yield c :: r 
  }

  // P28
  def lsort[A](lst: List[List[A]]): List[List[A]] = {
    lst.sortBy(_.length)
  }

  def lsortFreq[A](lst: List[List[A]]): List[List[A]] = 
    lst.groupBy(_.length).toList.sortBy(_._2.length) flatMap (_._2)
  
  
}


object ListTest extends App {
  import ListProblems._
  
  
  //P28
//  val result = lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//  println(result)
//  assert(result == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))

  println("Test passed!")
}






























