package odds

/*
object HyunhjlPrint {
  trait Printable[T] {
    def str(indent: Int = 0): String
    def str: String = str(0)
    def print(indent: Int = 0): Unit = println(str(indent))
    def print: Unit = print(0)
  }
  implicit def any2Printable(a: Any): Printable[Any] = new Printable[Any] {
    import collection._
    def name = a match {
      case a: Array[_] => "Array"
      case g: GenTraversableLike[_, _] => g.stringPrefix 
      case i: Iterator[_] => "Iterator"
      case _ => ""
    }
    object Iter {
      def unapply(a: Any): Option[GenIterable[_]] = a match {
        case a: Array[_] => Some(a.toIterable)
        case t: GenTraversableOnce[_] => Some(t.toIterable)
        case _ => None
      }
    }
    object Nested {
      def unapply(i: GenIterable[_]): Option[GenIterable[_]] = i match {
        case nested if i.exists{case Iter(j) =>true case _ =>false} => Some(nested)
        case _ => None
      }
    }
    def str(indent: Int = 0) = " " * indent + (a match {
      case Iter(i) if i.isEmpty => name + " <empty>"
      case Iter(Nested(i)) => name + "\n" + i.map(_.str(indent+2)).mkString("\n")
      case Iter(i) => name + i.map(_.toString).mkString("(", ", ", ")")
      case _ => a.toString
    })
  }
  
  def main(args: Array[String]) {
    List(Array(Vector(1.0,1.1), Vector(0d)), Array(Array("a", "b", "c"), Array("x", "y"))). print
  }
}
*/