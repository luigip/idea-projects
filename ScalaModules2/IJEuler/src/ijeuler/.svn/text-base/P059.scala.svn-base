package ijeuler


object P059 extends App {
  val codes = io.Source.fromFile("ijeuler/cipher1.txt").getLines.next().split(",").map(_.toInt)
  val as = for (a <- 'a' to 'z'; b <- 'a' to 'z'; c <- 'a' to 'z')  
           yield (List(a, b, c).mkString * (codes.size / 3 + 1), codes).zipped map (_^_)
  val res = as.maxBy(_.count(' '==)).sum
  println(res)
}