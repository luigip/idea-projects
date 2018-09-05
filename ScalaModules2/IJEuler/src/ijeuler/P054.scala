package ijeuler

object P054 extends App {

  object Rank extends Enumeration {
    val TWO   = Value("2")
    val THREE = Value("3")
    val FOUR  = Value("4")
    val FIVE  = Value("5")
    val SIX   = Value("6")
    val SEVEN = Value("7")
    val EIGHT = Value("8")
    val NINE  = Value("9")
    val TEN   = Value("T")
    val JACK  = Value("J")
    val QUEEN = Value("Q")
    val KING  = Value("K")
    val ACE   = Value("A")
  }

  object Suit extends Enumeration {
    val SPADES   = Value("S")
    val HEARTS   = Value("H")
    val DIAMONDS = Value("D")
    val CLUBS    = Value("C")
  }

  case class Card(rank: Rank.Value, suit: Suit.Value) extends Ordered[Card]{
    override def toString = "["+rank+suit+"]"
    def compare(that: Card) = that.rank compare this.rank
    def this(s: String) = this(Rank.withName(s.substring(0,1)), Suit.withName(s.substring(1,2)))
  }

  case class Hand(cards: Array[Card]) {
    override def toString = cards.mkString

    def handStrength = {
      val handTypes = List(straightFlush, straight, flush, others)
      handTypes.flatten.map{i => strength(i)} match {
        case Nil => 0
        case x => x.max
      }
    }

    def flush: Option[List[Int]] = if (cards.map(_.suit).distinct.length == 1)
                    Some(6 :: cards.map(_.rank.id).toList) else None

    def straight: Option[List[Int]] = {
      if ( cards.sliding(2).forall { case Array(x, y) => (y compare x) == 1 } )
        Some(List(5, cards(0).rank.id, 0, 0, 0, 0))
      else if ( cards.map(_.rank.id).toList == List(12, 3, 2, 1, 0) )
        Some(List(5, 3, 0, 0, 0, 0))
        else None
    }
    
    def straightFlush = if (straight.isDefined && flush.isDefined){
      if (cards(0).rank == Rank.ACE && cards(1).rank == Rank.FIVE){
        Some((9 :: cards.slice(1,5).map(_.rank.id).toList) :+ cards(0).rank.id) // steel wheel
      }
        else Some(9 :: cards.map(_.rank.id).toList)
    } else None

    def others = {
      val ranks = cards.map(_.rank.id)
      val counts = ranks.map(i => (i, ranks.count(_ == i))).distinct.sortWith(_._2 > _._2)
      counts.length match {
        case 2 if counts(0)._2 == 4 => Some(8 :: counts(0)._1 :: counts(1)._1 :: 0 :: 0 ::0 :: Nil) // 4 of a kind
        case 2                      => Some(7 :: counts(0)._1 :: counts(1)._1 :: 0 :: 0 ::0 :: Nil) // full house
        case 3 if counts(0)._2 == 3 => Some(4 :: counts(0)._1 :: counts(1)._1 :: counts(2)._1 :: 0 ::0 :: Nil) // trips
        case 3                      => Some(3 :: counts(0)._1 :: counts(1)._1 :: counts(2)._1 :: 0 ::0 :: Nil) // 2 pair
        case 4 => Some(2 :: counts(0)._1 :: counts(1)._1 :: counts(2)._1 :: counts(3)._1 :: 0:: Nil) // 1 pair
        case 5 => Some(1 :: counts(0)._1 :: counts(1)._1 :: counts(2)._1 :: counts(3)._1 :: counts(4)._1 :: Nil) // 0 pair
        case _ => None
      }
    }
  }

  def strength(xs: Seq[Int]) =
      1 * xs(5) + 14 * xs(4) + 196 * xs(3) + 2744 * xs(2) + 38416 * xs(1) + 537824 * xs(0)
    /*  1 = HighCard, 2 = OnePair, 3 = TwoPair, 4 = Trips, 5 = Straight, 6 = Flush,
        7 = FullHouse, 8 = Quads, 9 = StraightFlush                                 */

  case class MatchUp(hand1: Hand, hand2: Hand)  {
    override def toString = hand1 + " vs " + hand2
  }

  def parseHand(s: String) = Hand( s split(" ") map { j => new Card(j) } sorted)

  val matchups = {
    val lines = io.Source.fromFile("ijeuler/poker.txt").getLines.toList
    for {i <- lines
      left  = parseHand(i.substring(0,14))
      right = parseHand(i.substring(15,29))
    } yield MatchUp(left, right)
  }

  val strs = matchups.map {(i: MatchUp) => (i.hand1.handStrength, i.hand2.handStrength)}
  val res = strs.count{i => i._1 > i._2}
  println(res)
}

