package odds

object InterOpS {
  val list: List[java.lang.Integer] = List(1,2,3,4)
  val f = (a:Int, b: Int) => a + b
  
//  def x() = 2
//  def y   = 3
//  

}

class Foo {
  def bar = new Bar
}

class Bar {
  def apply(x: Int) = x
}