package odds

import collection.mutable.ListBuffer

object Splitter extends App {
  
  def split[T](xs: Seq[T])(ops: (T => Boolean, T => T)*): Seq[Seq[T]] = {
    val (filters, maps) = ops.unzip    
    val buffers = IndexedSeq.fill(ops.size)(ListBuffer.empty[T])
    for (x <- xs; i <- buffers.indices )
      if (filters(i)(x)) buffers(i) += maps(i)(x)
    buffers.map(_.toSeq)
  }
  
  val res = split(1 to 10)(
    (_ < 5, _ * 100),    // multiply everything under 5 by 100
    (_ %2 == 1, 0 - _),  // negate all odd numbers
    (_ % 3 == 0, _ + 5)  // add 5 to numbers divisible by 3
  )

  println(res) //Vector(List(100, 200, 300, 400), List(-1, -3, -5, -7, -9), List(8, 11, 14))

}