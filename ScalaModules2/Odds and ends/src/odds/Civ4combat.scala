package odds

import util.Random

object Civ4combat extends App {

  case class Battle(prob: Double, outcome: Int)
  
  val xs = io.Source.fromFile("odds and ends/combat.txt").getLines().toList
  val size = xs.size
  val rand = new Random

  // Transform number strings into Battle objects
  val battles = for {
    x <- xs
    Array(prob, outcome) = x.split("\\s")//Array("a","b")
  } yield Battle(prob.toDouble, outcome.toInt)

  // List of probabilities for all battles
  val probs = battles map (_.prob)

  // Number of simulated runs
  val N = 1000
  
  def simulate(probabilities: List[Double]) = for {
    p <- probabilities
    outcome = if (p > rand.nextDouble) 1 else 0
  } yield Battle(p, outcome)
  
  def wins(xs: List[Battle]) = xs.map(_.outcome).sum
 
  val sims: List[List[Battle]] = for {
    i <- (1 to N).toList
  } yield simulate(probs)
  
  val counts = sims map wins
  val actualCount = wins(battles)
  
  val p_value = 0.05
  
  val upper = {
    // rough! better for large N
    val sortedWins = counts.sorted
    val rank = ((1 - p_value/2) * N).toInt
    sortedWins(rank)
  }

  val lower = {
    // rough! better for large N
    val sortedWins = counts.sorted
    val rank = ((p_value/2) * N).toInt
    sortedWins(rank)
  }
  
  //println("Simulated % wins:")
  //counts.sorted foreach println
  println("Actual % wins:")
  println(actualCount/size.toDouble)
  println("Upper " + upper/size.toDouble)
  println("Lower " + lower/size.toDouble)
  
}