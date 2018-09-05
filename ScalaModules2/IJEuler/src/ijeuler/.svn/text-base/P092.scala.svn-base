package ijeuler


object P092 extends App {
  def endsAt89(n: Int): Boolean = n match {
    case 89 => true
    case 1  => false
    case _  => endsAt89(sum(n))
  }
  def sum(n: Int): Int = if (n == 0) 0 else (n % 10)*(n % 10) + sum(n / 10) 
  val res = (1 to 10000000) count endsAt89
  println(res)
}

/*
 * Faster version with caching - could probably be improved
 */
object P092b {
//  val t0 = System.currentTimeMillis()
  val a = new Array[Int](10000000+1)
  
  def endsAt89(n: Int): Boolean = n match {
    case 89 => true
    case 1  => false
    case x if a(x) != 0 => endsAt89(a(x))
    case _  => {
      if(endsAt89(sum(n))) {a(n) = 89; true}
      else {a(n) = 1; false}
    }
  }
  def sum(n: Int): Int = if (n == 0) 0 else (n % 10)*(n % 10) + sum(n / 10) 

  def main(args: Array[String]) {
    val res = (1 to 10000000) count endsAt89
    println(res)
//  println(System.currentTimeMillis() - t0)
  }
}

