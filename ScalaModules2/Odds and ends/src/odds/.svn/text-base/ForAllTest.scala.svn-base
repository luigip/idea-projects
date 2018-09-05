package odds

import collection.GenTraversable
import collection.parallel.ParSeq

object ForAlls {

  def forall2[A](s: IndexedSeq[A], p: A => Boolean):Boolean = {
    var i = 0
    while (i < s.length) {
      if (!p(s(i))) return false
      i += 1
    }
    true
  }

  def forall4[A](lst: List[A], p: A => Boolean):Boolean = {
    var these = lst
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }
}

object ForAllTest extends App {
//  import ForAlls._

  val r = (1 to 15)
  val rl = r.toList
  val ra = r.toArray
  val ri = r.toStream
  val rs = r.toSeq.par

  def isDivis(x:Int) = rs forall {x % _ == 0}

  def find(n:Int):Int = if (isDivis(n)) n else find (n+2)

  var t = System.nanoTime
  println (find (2))
  println((System.nanoTime - t).toDouble / 1e9)


  case class PimpedSeq[A](s: IndexedSeq[A]) {
    def forall2 (p: A => Boolean): Boolean = {
      var i = 0
      while (i < s.length) {
        if (!p(s(i))) return false
        i += 1
      }
      true
    }
  }
  implicit def seqToPimpedSeq[A](in: IndexedSeq[A]): PimpedSeq[A] = PimpedSeq(in)

  case class PimpedParSeq[A](s: ParSeq[A]) {
    def forall2 (p: A => Boolean): Boolean = {
      var i = 0
      while (i < s.length) {
        if (!p(s(i))) return false
        i += 1
      }
      true
    }
  }
  implicit def seqToPimpedSeq[A](in: ParSeq[A]): PimpedParSeq[A] = PimpedParSeq(in)
  
  
  case class PimpedList[A](lst:  GenTraversable[A]) {
    def forall4 (p: A => Boolean): Boolean = {
      var these = lst
      while (!these.isEmpty) {
        if (!p(these.head)) return false
        these = these.tail
      }
      true
    }
  }
  implicit def listToPimpedList[A](in: GenTraversable[A]): PimpedList[A] = PimpedList(in)
}

//hi

//hi again
//  def isDivis(x:Int) = (1 to 20) forall {x % _ == 0}
//  def isDivis(x:Int) =  forall2(ra, {i:Int => x % i == 0})













