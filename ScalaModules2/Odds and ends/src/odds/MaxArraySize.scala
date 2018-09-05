package odds

/**
 * Created by Rhys on 14/09/2015.
 */
object MaxArraySize extends App
{
  def findMax(highestWorking: Int, highestNotWorking: Int, current: Int): Int = {
    println(s"$highestWorking $highestNotWorking $current")
    if (current == highestWorking) highestWorking else
      try {
        val a = new Array[Byte](current)
        // no Error, so current is working
        findMax(current, highestNotWorking, (current+highestNotWorking+1)/2)
      }
      catch { case _: OutOfMemoryError => findMax(highestWorking, current, (current+highestWorking)/2) }
  }

  val result = findMax(0, Int.MaxValue, Int.MaxValue)
  println(result)
}
