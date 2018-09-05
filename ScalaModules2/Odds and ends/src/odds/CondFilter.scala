package odds

import collection.generic.FilterMonadic
//import language.implicitConversions

object CondFilter {

  class FilterMonadicHelper[A](val fm: FilterMonadic[A, List[A]]) {
    def ifFilter(cond: Boolean, f: A => Boolean): FilterMonadicHelper[A] =
      if (cond) new FilterMonadicHelper(fm.withFilter(f)) else new FilterMonadicHelper(fm)
  }
  
  implicit def toFilterMonadicHelper[A](xs: List[A]) = new FilterMonadicHelper(xs.withFilter(_ => true))
  
  implicit def fromFilterMonadicHelper[A](fmh: FilterMonadicHelper[A]) = fmh.fm map identity


  def main(args: Array[String]) {
    val list = List(1 to 10 :_*)
    
    val res: List[Int] = 
      list.ifFilter(list.length < 10, _ % 2 == 0)
          .ifFilter(list.length > 5, _ % 2 == 1)
          .ifFilter(true, _ < 7 )
    
    println(res)
  }
  
  
}