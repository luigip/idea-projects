package odds


object FoldsSpecial {

  /**
   * Folds a collection until a supplied condition is met
   *  
   * @param zero  The initial accumulator value
   * @param xs    The collection to fold over
   * @param op    Combination operation between accumulator and element
   * @param until Condition upon which to stop folding
   * @tparam A    Element type
   * @tparam B    Result type
   * @return      Either Right(result) or Left((accumulator, element) at time of stopping)
   */
  def foldUntil[A,B](zero: B, xs: Iterable[A])(op: (B,A) => B, until: (B,A) => Boolean)
                    : Either[(B,A),B] = { 
    val it = xs.iterator
    def iter(acc: B): Either[(B,A),B] =  
      if (!it.hasNext)     Right(acc)
      else {
        val x = it.next()
        if (until(acc, x)) Left((acc, x))
        else               iter(op(acc, x))
      }
    iter(zero)
  }
}

object FoldsSpecialTest extends App {
  import FoldsSpecial._
  
  // test
  val res = foldUntil(0, Vector(2,4,3,6))(_ + _, (acc, x) => x % 2 != 0)
             
  println(res)
  
}