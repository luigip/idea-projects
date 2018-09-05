package odds

/**
 * A class that implements while-loops lazily, by converting to an Iterator
 * 
 * Usage: 
 *   val it = 
 */
class Iter[A] private (cond: => Boolean)(src: => A) extends Iterator[A] {
  private var checked    = false
  private var hasNextVal = false
  def hasNext = if (checked) hasNextVal
                else { checked = true; hasNextVal = cond; hasNextVal }
  def next()  = if (hasNext) { checked = false; src }
                else Iterator.empty.next
}

object Iter {
  def apply[A](cond: => Boolean)(src: => A) = new Iter(cond)(src)
  
  def whileToList[A](cond: => Boolean)(src: => A) = {
    var lst = List[A]()
    while (cond) {
      lst = src :: lst
    }
    lst
  }
} 

object Test extends App {
  var a = 5
  val it = Iter(a > 0) {println(a); a -= 1}
  it foreach (i => i)
  util.Random.nextInt
}