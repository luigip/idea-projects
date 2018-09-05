def rec(n: Int, count: Int, target: Int): Int = {
  if (n.toString.length > target - count) n.toString.charAt(target - count).asDigit
  else rec(n + 1, count + n.toString.length, target)
}

val res = List(1,10,100,1000,10000,100000,1000000).map(i => rec(1, 1, i)).product

println(res)