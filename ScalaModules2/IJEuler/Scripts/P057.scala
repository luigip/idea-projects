def rec(num: BigInt, den: BigInt, term: Int): (BigInt, BigInt) = {
  if (term == 0) {
    val n = num - den
    val g = hcf(n, den)
    (n / g, den / g)
  }
  else {
    val nn = den +  num * 2
    val g = hcf(nn, num)
    rec(nn / g, num / g , term - 1)
  }
}



def hcf(a: BigInt, b: BigInt):BigInt = if(b == 0) a else hcf(b, a - b * (a / b))


val res = for {
  n <- 1 to 1000
  t = rec(2,1,n)
//  a = println(n + " " + t)
  if t._1.toString.length > t._2.toString.length
} yield t
//println(res)
println(res.length)















