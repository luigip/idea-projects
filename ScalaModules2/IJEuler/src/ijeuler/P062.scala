package ijeuler

import collection.mutable.HashMap

object P062 extends App {
  
  val cubes = HashMap[String, List[Long]]()
  for {
    i <- Range.Long(0, 10000, 1)
    c = i * i * i
    digits = c.toString.sorted
  } cubes += (digits -> (c :: cubes.getOrElse(digits, List[Long]())))
  
  val res = cubes.filter(_._2.length == 5).minBy(_._2.min)._2.min
  
  println(res)
  
}
