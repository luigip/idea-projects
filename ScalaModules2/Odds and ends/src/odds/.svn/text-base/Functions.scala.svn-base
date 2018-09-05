package odds

object Functions {
  import scala.reflect.Manifest
  private var functions: List[(Manifest[_],Any,String)] = List()
  def add[T](desc: String, func: T)(implicit m: Manifest[T]) {
    functions ::= (m,func,desc)
  }

  def get[T]()(implicit m : Manifest[T]): List[(T,String)] = {
    functions flatMap {
      case (t,f,s) => if (t <:< m) Some(f.asInstanceOf[T],s) else None
    }
  }

  def getInputs[T]()(implicit m : Manifest[T]): List[(T => Any,String)] = {
    functions flatMap {
      case (t,f,s) => if (t <:< manifest[T => Any]) Some(f.asInstanceOf[T => Any],s) else None
    }
  }

  def main(args: Array[String]) {
    
    add("plus one", (_: Int) + 1)
    add("to double", (_: Float).toDouble)
    
    val fsTakingInt = getInputs[Int]()
    val floatToDoubles = get[Float => Double]()
    
    for ((f, desc) <- floatToDoubles ) {
      println(desc + " " + f(10f))
    }
    
  }
  
  
}