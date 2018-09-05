package odds2

object Josephus_Imperative extends collection.mutable.ArrayBuffer[Int] with App {
  val number = 40
  val step = 3
  this ++= 1 to number
  var pos = 0
  while (size > 1) { 
    pos = (pos + step - 1) % size
    remove(pos) 
  }
  println("Winner: " + this(0))
}

object Josephus_Immutable extends App {

  case class Person(index: Int)
  
  class Circle(step: Int, inputps: Seq[Person]) {
    val ps = Vector() ++ inputps
    def nextCircle = { 
      val pos = (step - 1) % ps.size
      val newps = ps.splitAt(pos) match { case (pre, post) => post.tail ++ pre }     
      new Circle(step, newps)
    }    
    def findWinner: Person = if (ps.size == 1) ps(0) else nextCircle.findWinner
  }

  val c = new Circle(3, 1 to 40 map Person)
  println("Winner: " + c.findWinner)  
  
}




// This one's basically the same as the Actors solution, just with method invocation
// rather than message passing
object Josephus_OO extends App {
  val N = 40
  val step = 3
  
  case class Person(id: Int) {
    var next: Person = _
    
    def receive(number: Int, sender: Person) {     
      if (sender == this) println("Winner: " + id)
      else if (number == 3) {
        sender.next = next
        next.receive(1, sender)
      }
      else next.receive(number + 1, this)
    }
  }

  val ps = 1 to N map Person
  ps foreach { p =>
    p.next = if (p.id == N) ps(0) else ps(p.id)
  }  
  ps.head.receive(1, null)  
}













