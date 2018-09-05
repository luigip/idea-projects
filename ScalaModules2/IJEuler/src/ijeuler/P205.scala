package ijeuler


object P205 extends App {

  def probs(dice: Int, faces: Int, out: Map[Int, Double] = Map((0, 1d))): Map[Int, Double] = dice match {
    case 0 => out
    case x => probs(dice - 1, faces, listToMap( for {
      (i, p) <- out.toList
      f      <- 1 to faces
    } yield (i + f, p / faces)))
  }

  def listToMap(lst: List[(Int, Double)]): Map[Int, Double] = {
    (Map[Int, Double]() /: lst) { case (a, (k,v)) => a.updated(k, a.getOrElse(k, 0d) + v) }
  }
  
  def cumulative(m: Map[Int, Double]) = m.toList.sorted.scanLeft((0, 0d)){
    case ((i,p), (k,v)) => k -> (p + v)
  }.toMap
  
  val pete  = probs(9, 4)
  val colin = probs(6, 6)
  val res = pete.map { case (i, p) => p * cumulative(colin)(i - 1) } .sum
  
  println("%.7f" format (res + 0.00000005))
}