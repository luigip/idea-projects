package odds

object ScrabbleCalculator {
  val tiles = Map[Char, Int](
    'a' -> 1,
    'b' -> 3,
    'c' -> 3,
    'd' -> 2
    // etc.
  )

  def main(args: Array[String]) {
    val out = (Vector[String]()
      :+ ""
      :+ "Scrabble Calculator 1.0"
      :+ "Enter words on the commandline."
      :+ "Use a '_' character for blank tiles."
      :+ ""
      ++ args.map {
        case w if w.length < 2 => w + ": one-letter words disallowed in Scrabble"
        case w => w + ": " + (w map tiles).sum + " points"
      }
    )
    out foreach println
    
  }

}