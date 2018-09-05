package odds

object Parens extends App {

  def balance2(chars: List[Char]): Boolean = chars.foldLeft(0){
    case (0, ')') => return false
    case (x, ')') => x - 1
    case (x, '(') => x + 1
    case (x, _  ) => x
  } == 0

  val res = balance2("a((cv))v(()()(())))".toList)

  println(res)
}