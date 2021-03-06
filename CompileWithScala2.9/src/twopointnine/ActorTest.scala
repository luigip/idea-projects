package twopointnine

object Worker extends Actor {
  println("started worker")
  def act() {
    while (true) {
      react {
        case s: String => println("Printing string: " + s)
        case lst: List[_] => lst foreach println
        case n: Int => println(n + " is an Int")
      }
    }

  }
}

object Manager extends Actor {
  var small = List[Int]()
  println("started worker")
  
  def act() {
    while (true) {
      receive {
        case n: Int if n < 10 => {
          if (small.size < 6)
            small = n +: small
          else
            Worker ! small
        }
        case n => Worker ! n
      }
    }
  }
}

object ActorTest extends App {
  println("start")
  Thread.sleep(3000)
  Worker.start()
  Manager.start()

  sl("Hi")
  sl(2)
  sl(3)
  sl(11)
  sl(List("yo","go","no"))
  sl(4)
  sl(5)
  sl(1)
  sl(7)
  sl(7)
  println("fin")

  def sl(a: Any) {
    Manager ! a
    Thread.sleep(1000)}
}