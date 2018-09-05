package odds
import language.implicitConversions

object JValues {
  trait JValue
  case class JInt(x: Int) extends JValue
  case class JString(x: String) extends JValue
  implicit def intToJInt(x: Int) = JInt(x)
  implicit def stringToJString(x: String) = JString(x)
}

object JValueTest extends App {

  import JValues._
  
  trait JList {
    def ::(x: JValue) = new NotEmpty(x, this)
  }
  case class NotEmpty(head: JValue, tail: JList) extends JList
  case object JNil extends JList
  
  implicit def jListToList(xs: JList): List[JValue] = xs match {
    case JNil => Nil
    case NotEmpty(y, ys) => new scala.::(y, ys)
  }

  val jl1 = 42 :: "abc" :: 123 :: JNil
  val jl: List[JValue] = 42 :: "abc" :: 123 :: JNil
  
  jl1
  jl
  
  println(jl)
}

object JValueTest2 extends App {
  import JValues._
  
  implicit class pimp(xs: List[JValue]) {
    def |: (x: JValue) = x :: xs
  }
  val foo = 123 |: "hello" |: Nil
  
  val bar = 11 |: 41 |: 42 |: Nil
  
  bar
  foo
  
  println(foo)
}