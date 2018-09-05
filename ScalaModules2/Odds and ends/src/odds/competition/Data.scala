package odds.competition

/**
  * Created by Rhys on 22/12/2015.
  */
object Data {
  case class Entry(name: String, score: Int)
  val maxScore = 75
  val competitors: List[Entry] = List(
    Entry("Smith", 75),
    Entry("Jones", 74),
    Entry("Doe", 30),
    Entry("Bloggs", 22)
  )
}
