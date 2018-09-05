package ijeuler

object Pascal extends App {
  def fact(b: BigInt):BigInt = if(b == 0) 1 else b * fact(b - 1)
  def C(n: Int, k:Int) = fact(n) / fact(k) / fact (n - k)
  def row(n: Int) = (0 to n).map(C(n, _)).toList
  def triangle(MAX:Int) = (0 to MAX).map(row(_)).toList
  println(triangle(14))
}





































