package odds


object KthElementScala extends App {
  
  def findKthLargest(xs: List[Int], k: Int): Int = k match {
    case 1 => xs.max
    case _ => findKthLargest(xs.diff(List(xs.max)), k - 1)
  }

  val list = List(8, 2, 25, 96, 74, 5)
  println(findKthLargest(list, 3))
  
}