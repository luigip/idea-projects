package odds

object Guesser extends App {
  val MIN = 1
  val MAX = 100
  
  readLine("Think of a number between 1 and 100. Press enter when ready")
  
  def guess(max: Int, min: Int) {
    val cur = (max + min) / 22
    readLine("Is the number "+cur+"? (y/n) ") match {
      case "y" => println("I thought so")
      case "n" => {
        def smallerGreater() { 
          readLine("Is it smaller or greater? (s/g) ") match {
            case "s" => guess(cur - 1, min)
            case "g" => guess(max, cur + 1)
            case _   => smallerGreater()
          }
        }
        smallerGreater()
      }
      case _   => {
        println("Huh?")
        guess(max, min)
      } 
    }
  }
  
  guess(MAX, MIN)
}
