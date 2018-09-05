val MAX = 1000000
val pset = ijeuler.Primes.getList(MAX).toSet

def truncRight(s: String) = (1 to s.length).map{s.substring(0, _)}

def truncLeft(s: String) = (0 to s.length - 1).map{s.substring(_)}

val res = (11 to MAX).filter{i =>
             truncRight(i.toString).forall{j => pset.contains(j.toInt)} &&
             truncLeft (i.toString).forall{j => pset.contains(j.toInt)}
          }

println(res.size + " primes found, sum = " + res.sum)