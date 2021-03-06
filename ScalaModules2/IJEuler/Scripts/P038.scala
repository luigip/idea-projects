def rec(n: Int, i: Long, result: String):Long = {
  val r = result + (n * i).toString
  if (r.length >= 10) result.toLong
  else rec(n + 1, i, r)
}

val OneToNine = Range(1,10).toSet
def isPandigital(n: Long) = n.toString.map(_.asDigit).toSet == OneToNine

val res = (1 to 9999).map{i => rec(1, i, "")}.filter{isPandigital}.max

println(res)
