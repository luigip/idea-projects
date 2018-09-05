package ijeuler
object P206b extends App {
  val pow10 = (0 to 18).map(i => math.pow(10,i).toLong).toArray
  def digit(n: Long, i: Int) = n / pow10(i) % 10
  
  def isIt(n: Long) = (1 to 9) forall {i => i == digit(n, 2 * (10 - i)) }
  
//  val t0 = System.currentTimeMillis()
  
//  val max = math.sqrt(1929394959697989990L).toLong
  val min = math.sqrt(1020304050607080900L).toLong / 10 * 10

  def go(i: Long): Long = if (isIt(i * i)) i
                          else go(i + 10)
  
  println(go(min))
//  println("took : " + (System.currentTimeMillis - t0) + " ms")
}