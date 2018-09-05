/**
 * Created by Rhys on 31 May 2015.
 */
object Launcher extends App {

  val example1 = Vector(
      "    2 5  ",
      "2 68     ",
      "5     97 ",
      "9 37   64",
      "         ",
      "15   27 8",
      " 75     9",
      "     83 7",
      "  2 4    ")

  val test = new Grid(example1)

  test.solve()
  test.result foreach println
}
