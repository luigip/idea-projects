package odds

object MininalLife extends App {

  def vicinity (p: (Int, Int)) = for { dx <- Set(-1,0,1)
                                       dy <- Set(-1,0,1) } yield (p._1 + dx, p._2 + dy)

  def next (ps: Set[(Int, Int)]) = ps flatMap vicinity filter { p =>
    val ns = vicinity(p) - p count ps
    ns == 3 || ns == 2 && ps(p)
  }

  def display (width: Int, height: Int, ps: Set[(Int, Int)]) =
    Seq.tabulate(height, width) { (y, x) => if (ps(x, y)) 'X' else '.' } map (_.mkString)

  val g = Set((2, 0), (2, 1), (2, 2), (1, 2), (0, 1))
  val frames = Seq.iterate(g, 9)(next)
  frames foreach { f => display(5, 5, f) foreach println; println(); Thread.sleep(500) }
}