package twopointnine

// This works, although on reflection Actors aren't a great match for this problem,
// since their point is that they act concurrently, and the concurrency actually makes this
// more difficult (cf next solution)
object Josephus_Actors extends App {
  
  val N = 40
  val step = 3
  
  case class Person(id: Int) extends Actor { Self =>
    // would like to make `next` private, but it's then very tricky to set up
    var next: Person = _
    private[this] var alive = true
    def act {
      while (alive) {
        // Messages consist of (count, sender) where count is distance from last man out
        receive {
          // last man standing
          case (_, Self) => {
            println("Winner: "+id)
            sys.exit(0)
          }          
          // kill self
          case (x: Int, _) if x == step => {
            alive = false
            reply {next}
          }
          // kill next 
          case (x: Int, _) if x == step - 1 => {
            // wait until we have received our next value
            next !? ((step, Self)) match {
              case a: Person => next = a
              case _ =>                
            }
            next ! (1, Self)
          }
          /* pass on message */
          case (x: Int, _) => {
            next ! (x + 1, Self)
          }
          case other => println(id + " unrecognized message: " + other)
        }
      }
    }
  }
  
  val acts = 1 to N map Person
  acts foreach { p =>
    p.next = p.id match { 
      case N => acts(0)
      case x  => acts(x)
    }
    p.start()
  }
  acts(0) ! (1, null)  
}