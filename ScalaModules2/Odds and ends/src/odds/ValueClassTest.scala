package odds

/**
 * Created by Rhys on 10/09/2015.
 */
object ValueClassTest extends testingRJ.Benchmark {

  implicit class Pipe[T](x: T) {  // 130, 129, 129, 129
    def |> [U](f: T=>U): U = f(x)
  }

//  implicit class Pipe[T](val x: T) extends AnyVal {  // 136, 147, 130, 130
//    def |> [U](f: T=>U): U = f(x)
//  }

  var dummy = 0

  val f: Int => Int = _ + 1

  def run(): Unit = {

    dummy = dummy |> f

  }
}
