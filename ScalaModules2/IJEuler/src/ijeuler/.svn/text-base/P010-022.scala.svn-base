package ijeuler

//object P001Alt extends App {
//  def arithProg(a:Int, d:Int, n:Int): Long = n * (2 * a + (n - 1) * d.toLong) / 2;
//  def find(n: Int):Long = arithProg (3, 3, n/3) + arithProg (5, 5, n/5) - arithProg (15, 15, n/15);
//  println(find(1000000000 - 1))
//}

object P010 extends App {
  val list = Primes getList 2000000
  val total = list.foldLeft(0L)(_ + _)
  println(total)
}

object P011 extends App {
  val data =
  """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""

  val nums = data.split("[\\r\\n\\s]+").map(_.toInt)
  val dright = (for {
    i <- 0 until 400
    x = i % 20
    if (x < 16)
  } yield {
    nums(i) * nums(i + 1) * nums(i + 2) * nums(i + 3)
  }).max
  val ddown = (for {
    i <- 0 until 400
    y = i / 20
    if (y < 16)
  } yield {
    nums(i) * nums(i + 20) * nums(i + 40) * nums(i + 60)
  }).max
  val ddiag1 = (for {
    i <- 0 until 400
    x = i % 20
    y = i / 20
    if (x < 16 && y < 16)
  } yield {
    nums(i) * nums(i + 21) * nums(i + 42) * nums(i + 63)
  }).max
  val ddiag2 = (for {
    i <- 0 until 400
    x = i % 20
    y = i / 20
    if (x > 3 && y < 16)
  } yield {
    nums(i) * nums(i + 19) * nums(i + 38) * nums(i + 57)
  }).max
  println((ddown :: dright :: ddiag1 :: ddiag2 :: Nil).max)
}

object P012 extends App {
  def nDivisors(n: Int) = (for {
    i <- 1 to math.sqrt(n).toInt
    if (n % i == 0)
  } yield i).length * 2                  // double for reciprocals
  
  def nextTriangle(n: Int, i:Int):Int = {
    if(nDivisors(n) > 500) n
    else nextTriangle(n + i + 1, i + 1)
  }
  val ret = nextTriangle(1, 1)
  println(ret)
}

object P013 extends App {
  import scala.io.Source.fromFile
  val ret = fromFile("IJEuler/src/P013_nums.txt").getLines().
              map(x => BigInt(x)).sum.toString().slice(0,10)
  println(ret)
}

object P014 extends App {
  def next(n: Long, i: Int):Int = {
    if (n == 1) i
    else next(f(n), i + 1)
  }
  def f(x:Long) = {
    if (x % 2 == 0) x / 2
    else 3 * x + 1
  }
  val ret = (for (i <- 1 until 1000000) yield (i, next(i, 1))).
    reduceLeft((a, b) => if (a._2 > b._2) a else b)._1

  println(ret)
}

object P015 extends App {
  def line(vals: List[Long], i:Int, size: Int):List[Long] = {
    if(i == size * 2) vals
    else line(List(1L) ++ (vals.sliding(2).toList.map(a => a(0)+a(1))) ++ List (1L), i + 1, size)
//    else line((0L::vals).reverse.zip(0L::vals).map(v => v._1 + v._2), i + 1, size)
  }
  val SIZE = 20
  val result = line(List[Long](1,1),1,SIZE)(SIZE)
  println(result)
}

object P016 extends App {
  val result = BigInt(2).pow(1000).toString.map(_.asDigit).sum
  println(result)
}

object P018 extends App {
  val lines= io.Source.fromFile("src/P018.txt").getLines()
               .map(_.split(" ").map(_.toInt).toList).toList

  def combineRows(r1: List[Int], r2: List[Int] ): List[Int] = r1 match {
    case Nil       => Nil
    case h :: tail => (h + (r2(0) max r2(1))) :: combineRows (r1.tail, r2.tail)
  }

  val result = lines.reduceRight(combineRows(_,_)) (0)
  println(result)
}

object P019 extends App {
  val months = Array(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def isLeapYear(year: Int) = (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0)

  def howMany(year: Int, month: Int, dayOfWeek: Int, count: Int): Int = {
    if (year > 2000) count
    else {
      val daysInMonth = if ((month == 2) && isLeapYear(year)) 29 else months(month)
      val dc = if (dayOfWeek == 0 && year >= 1901) 1 else 0

      month match {
        case 12 => howMany(year + 1, 1,         (dayOfWeek + daysInMonth) % 7, count + dc)
        case _  => howMany(year,     month + 1, (dayOfWeek + daysInMonth) % 7, count + dc)
      }
    }
  }

  val result = howMany(1900, 1, 1, 0)
  println(result)
}

object P020 extends App {
  val result = (BigInt(1) to BigInt(100)).reduceLeft(_*_).toString.map(_-'0').sum
}

object P021 extends App {
  def d(n: Int) = (1 until n).filter(n % _ == 0).sum
  def amic(n: Int) = (1 until n).filter(i => d(i) != i && i == d(d(i)))
  val result = amic(10000).sum
  println(result)
}

object P022 extends App {
  val names = io.Source.fromFile("P022.txt").getLines.next().replace("\"","").split(",").sorted
  def value(s: String) = s.map(_ - 'A' + 1).sum
  val result = (1 to names.length).map(i => value(names(i - 1)) * i).sum
  println(result)
}


