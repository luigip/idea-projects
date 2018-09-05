package ijeuler


object P099 extends App {
  val xs = for {
    (pair, i) <- io.Source.fromFile("ijeuler/base_exp.txt").getLines().zipWithIndex
    Array(b, e) = pair.split(",").map(_.toInt)
  } yield (e * math.log(b), i)
  val res = xs.maxBy(_._1)._2 + 1
  println(res)
}

//// imperative version with foreach
//object P099b extends App {
//  var(maxn, maxi) = (0d, -1)
//  io.Source.fromFile("ijeuler/base_exp.txt").getLines().toSeq.zipWithIndex foreach {
//    case (pair, i) =>
//    val Array(b, e) = pair.split(",").map(_.toInt)
//    val s = e * math.log(b)
//    if(s > maxn) {maxn = s; maxi = i}
//  }
//  println(maxi + 1)
//}
//
//// imperative with for-expression, using indices
//object P099c extends App {
//  val input = io.Source.fromFile("ijeuler/base_exp.txt").getLines().toSeq
//  var(maxn, maxi) = (0d, -1)
//  for { i <- input.indices
//        Array(b, e) = input(i).split(",").map(_.toInt)
//        res = e * math.log(b) 
//        if(res > maxn)
//  } {maxn = res; maxi = i}
//  println(maxi + 1)
//}

// Using maps instead of for-expression sugar
object P099d extends App {
  val res = io.Source.fromFile("ijeuler/base_exp.txt").getLines().zipWithIndex
    .map { case (pair, i) =>
      val Array(b, e) = pair.split(",").map(_.toInt)
      (e * math.log(b), i)                           
    }.maxBy(_._1)._2 + 1
  
  println(res)
}

// Recursive
object P099e extends App {
  def find(lst: List[String], i: Int = 1, maxn: Double = 0, maxi: Int = -1): Int = lst match {
    case Nil => maxi
    case x :: xs => {
      val Array(b, e) = x.split(",").map(_.toInt)
      val s = e * math.log(b)
      if (s > maxn) find(xs, i + 1, s, i)
      else          find(xs, i + 1, maxn, maxi) 
    }
  }
  val res = find(io.Source.fromFile("ijeuler/base_exp.txt").getLines().toList) 
  println(res)
}