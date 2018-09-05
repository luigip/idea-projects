def fact(n: Int) = (1 /: (1 to n))(_*_)

def maxNum(total: Int): Int = {
  if (total < fact(9) * total.toString.length) maxNum (total * 10 + 9)
  else (total - 9) / 10
}

def sumFactDigits(n : Int) = n.toString.map(i => fact(i.asDigit)).sum

val res = (3 to maxNum(9)).filter {i => i == sumFactDigits(i)}.sum
println(res)
