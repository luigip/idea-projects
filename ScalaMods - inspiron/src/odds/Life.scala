package odds

object Life extends App {

  case class Point private(x: Int, y: Int) { p =>

    lazy val neighbours: Set[Point] = neighbourCoOrds map {
      case (a, b) => Point.get(a + x, b + y)
    }

    def update(ps: Set[Point]) = neighbours count ps match {
      case 3          => Some(p)
      case 2 if ps(p) => Some(p)
      case _          => None
    }
  }

  object Point {
    private val cache = collection.mutable.Map.empty[(Int, Int), Point]
    def get(x: (Int, Int)) = cache.getOrElseUpdate(x, Point(x._1, x._2))
  }

//  def ns(p: Point) = neighbourCoOrds map { case (a, b) => Point.get(a + p.x, b + p.y) }

  val neighbourCoOrds = Set((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

//  def update(ps: Set[Point])(p: Point) = p.neighbours count ps match {
//      case 3          => Some(p)
//      case 2 if ps(p) => Some(p)
//      case _          => None
//  }

  def next(ps: Set[Point]) = {
    val toCheck = ps ++ (ps flatMap (_.neighbours))
    toCheck flatMap (_.update(ps))
  }

  def display(width: Int, height: Int, xs: Set[Point]) = {
    Seq.tabulate(height, width) {
      (y, x) => if (xs(Point.get(x, y))) "X" else "."
    } map (_.mkString)
  }

  def grid(xs: (Int, Int)*) = xs.toSet map Point.get


  ////////////////
  val g = grid((2, 1), (3, 1), (4, 1))
  val frames = Seq.iterate(g, 5)(next)

  frames foreach {
    f => display(10, 5, f) foreach println; println()
  }
}