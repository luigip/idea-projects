package ijeuler


object P081 extends App {
  
val m = io.Source.fromFile("ijeuler/matrix.txt").getLines().toArray.map(_.split(",").map(_.toInt))
val yn = m.size
val xn = m(0).size

for { i <- 1 until yn } m(i)(0) += m(i - 1)(0)
for { i <- 1 until xn } m(0)(i) += m(0)(i - 1)
for { y <- 1 until yn 
      x <- 1 until xn } m(y)(x) += m(y - 1)(x) min m(y)(x - 1)

println(m.last.last)
}