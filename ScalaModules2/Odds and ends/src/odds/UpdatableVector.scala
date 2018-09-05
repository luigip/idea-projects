package odds

object UpdatableVector {
  implicit def vectorToUpdatableVector2[T](v: Vector[Vector[T]]) = new UpdatableVector2(v)
  implicit def vectorToUpdatableVector3[T](v: Vector[Vector[Vector[T]]]) = new UpdatableVector3(v)
  implicit def vectorToUpdatableVector4[T](v: Vector[Vector[Vector[Vector[T]]]]) = new UpdatableVector4(v)

  class UpdatableVector2[T](v: Vector[Vector[T]]) {
    def updated2(c1: Int, c2: Int)(newVal: T) =
      v.updated(c1, v(c1).updated(c2, newVal))
  }

  class UpdatableVector3[T](v: Vector[Vector[Vector[T]]]) {
    def updated3(c1: Int, c2: Int, c3: Int)(newVal: T) =
      v.updated(c1, v(c1).updated2(c2, c3)(newVal))
  }

  class UpdatableVector4[T](v: Vector[Vector[Vector[Vector[T]]]]) {
    def updated4(c1: Int, c2: Int, c3: Int, c4: Int)(newVal: T) =
      v.updated(c1, v(c1).updated3(c2, c3, c4)(newVal))
  }
}

object Main extends App {

  import UpdatableVector._
  
  val v2 = Vector.fill(2,2)(0)
  val r2 = v2.updated2(1,1)(42)
  println(r2)
  
  val v3 = Vector.fill(2,2,2)(0)
  val r3 = v3.updated3(1,1,1)(42)
  println(r3)

  val v4 = Vector.fill(2,2,2,2)(0)
  val r4 = v4.updated4(1,1,1,1)(42)
  println(r4)

}
