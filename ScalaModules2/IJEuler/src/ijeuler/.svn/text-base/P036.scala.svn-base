def isPalindromic (n: Int) = {
  val s = Integer.toString(n, 10)
  val t = Integer.toString(n, 2)
  s == s.reverse && t == t.reverse
}

val res = (1 until 1000000).filter{isPalindromic}.sum

println(res)
