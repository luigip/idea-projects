package odds.competition
import Data.{competitors, maxScore, Entry}
import scala.util.Random

/**
  * Created by Rhys on 22/12/2015.
  */

// Works by replacing score with a cumulative number of correct answers,
// then picking a number and searching for that entry.
// Efficient "dartboard"
object WinnerPicker1 extends App {
  val cumulative = competitors.scanLeft(Entry("", 0)){
    case (Entry(_,total), Entry(name, score)) => Entry(name, score + total)
  }
  val grandTotal = competitors.map(_.score).sum
  val winner = Random.nextInt(grandTotal)
  val winnerName = cumulative.find({case Entry(name, cum) => cum > winner}).get.name

  //println(winner)
  //println(cumulative)
  println(winnerName)

}

// Same dartboard ideas, but simpler: makes an actual array with [name], [score] times, and picks random element
// Though less efficient computationally
object WinnerPicker2 extends App {
  val names = competitors.flatMap({ case Entry(name, score) => Array.fill(score)(name)}).toArray
  val grandTotal = names.length
  val winnerIndex = Random.nextInt(grandTotal)
  val winner = names(winnerIndex)
  println(s"Winner Index = $winnerIndex \nWinner = $winner")

}

// Method that does not require marking in advance
// 1. Pick an entry at random
// 2. Chance of winning is proportional to score as % of max
// 3.
object WinnerPicker3 extends App {
  def winner: String = {
    val index: Int = Random.nextInt(competitors.size)
    val picked: Entry = competitors(index)
    val percent: Double = picked.score.toDouble / maxScore
    val winCheck = Random.nextDouble() // 0.0 to 1.0
    if(winCheck < percent) picked.name
      else winner
  }
  println(winner)
}
