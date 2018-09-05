
def isLychrel(n: Int) = {
  lazy val s: Stream[BigInt] = n #:: s.map(i => i + BigInt(i.toString.reverse))
  s.take(50).tail.forall(b => b != BigInt(b.toString.reverse))
}

val res = (1 until 10000).filter(isLychrel).length
println(res)








// The two methods take roughly the same time, since Stream elements not evaluated unless needed

/*

def isPalindrome(b: BigInt) = b == BigInt(b.toString.reverse)

def isLych(n: BigInt, i: Int): Boolean = i match {
  case 50 => true
  case _ if i > 1 && isPalindrome(n) => false
  case _ => isLych(n + BigInt(n.toString.reverse), i + 1)
}

val res = (1 until 10000).filter(n => isLych(n, 1)).length

*/