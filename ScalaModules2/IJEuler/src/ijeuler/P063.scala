package ijeuler


object P063 extends App {
  implicit def power[T <% BigInt](a: T) = new {
   def **(b: Int) = a pow b
  }
  
  var n = Set[BigInt]()
  (1 to 9).foreach { i =>
    var j = 0
    var res = BigInt(0)
    while (j-1 <= res.toString.size) {
      res = i ** j
//      println("i = " + i + " j = " + j + " res = " + res)
      if(res.toString.size == j) n += res
      j+=1
    }
  }
  println(n.size)
}