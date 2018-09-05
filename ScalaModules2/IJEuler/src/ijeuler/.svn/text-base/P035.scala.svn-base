def rots(n: Int) = {
  val s = n.toString
  for (i <- (1 to s.length)) yield (s.substring(i) + s.substring(0,i)).toInt
}

val pset = ijeuler.Primes.getList(999999).toSet
val res = pset.filter{rots(_).forall{pset.contains(_)}}.size

println(res)