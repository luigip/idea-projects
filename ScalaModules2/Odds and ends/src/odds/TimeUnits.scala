package odds

object TimeUnits {
  implicit def hour2Minute(hour: Hour) = Minute(hour.num * 60)
  implicit def day2Hour(day: Day) = Hour(day.num * 24)
  implicit def day2Minute(day: Day) = Minute(day.num * 24 * 60)
}

trait TimeUnit

case class Day(val num: Int) extends TimeUnit {
  def - (sub: Day) = Day(num - sub.num)
  def + (sub: Day) = Day(num + sub.num)
}
case class Hour(val num: Int) extends TimeUnit {
  def - (sub: Hour) = Hour(num - sub.num)
  def + (sub: Hour) = Hour(num + sub.num)
}
case class Minute(val num: Int) extends TimeUnit {
  def - (sub: Minute) = Minute(num - sub.num)
  def + (sub: Minute) = Minute(num + sub.num)
}

object TimeUnitsTest extends App {
  import odds.TimeUnits._

  println(
    Hour(2) - Minute(30) // Minute(90)
  , Minute(90) - Hour(1)
  , Minute(90) + Hour(1)
  , Hour(2) + Minute(30)
  , Hour(2) - Hour(1)
  , Day(2) - Minute(2)
  )
}