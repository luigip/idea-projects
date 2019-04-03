package code

object S99Logic {

  def not(a: Boolean) = a match {
    case false          => true
    case _              => false
  }

  def table2(f:(Boolean, Boolean) => Boolean) {
    println("A     B     result")
    println("true  true  " + f(true, true))
    println("true  false " + f(true, false))
    println("false true  " + f(false, true))
    println("false false " + f(false, false))
  }
  implicit def b2b(b: Boolean) = new myBoolean(b)

  val gray: Stream[List[String]] = {
    List("") #:: List("0", "1") #::
      gray.tail.map { ls => ls.map {"0" + _} ++ ls.reverse.map {"1" + _} }
  }
}

class myBoolean(a: Boolean) {
  import S99Logic._

  def or(b: Boolean) = (a, b) match {
    case (false, false) => false
    case _              => true
  }

  def and(b: Boolean) = (a, b) match {
    case (true, true)   => true
    case _              => false
  }

  def nand(b: Boolean) = not(a and b)
  def nor (b: Boolean) = not(or(b))
  def xor (b: Boolean) = not(a equ b)
  def impl(b: Boolean) = not(a) or b
  def equ (b: Boolean) = (a and b) or (not(a) and not(b))
}

object test extends App {
  import S99Logic._

  table2((a: Boolean, b: Boolean) => a nand b)

}