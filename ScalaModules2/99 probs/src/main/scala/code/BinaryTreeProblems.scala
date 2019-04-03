package code

//////// PROVIDED ////////
sealed abstract class Tree[+T] {
  def isMirrorOf[U](that: Tree[U]): Boolean = (that, this) match {
    case (End, End) => true
    case (Node(v1, left1, right1), Node(v2, left2, right2)) => left1.isMirrorOf(right2) && right1.isMirrorOf(left2)
    case _ => false
  }
  def isSymmetric: Boolean = this match {
    case End => true
    case Node(value, left, right) => left isMirrorOf right
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
//////////////////////////

object Tree {
  def cBalanced[A](n: Int, a: A): List[Tree[A]] = n match {
    case 0 => List(End)
    //case 1 => List(Node(a, End, End))
    case x if n % 2 == 1 => {
      val nodes = cBalanced((n-1)/2, a)
      for {
        node1 <- nodes
        node2 <- nodes
      } yield Node(a, node1, node2)
    }
    case x if n % 2 == 0 => {
      val nodes1 = cBalanced((n-1)/2, a)
      val nodes2 = cBalanced(n/2, a)
      val greaterRight = for {
        node1 <- nodes1
        node2 <- nodes2
      } yield Node(a, node1, node2)
      val greaterLeft = for {
        node1 <- nodes1
        node2 <- nodes2
      } yield Node(a, node2, node1)
      greaterRight ++ greaterLeft
    }
  }
  


}

object Test0 extends App {
  val res = Tree.cBalanced(9, "x")
  
  res foreach {
    t => println(t, t.isSymmetric)
  }
  
  println(Node('a', Node('b'), Node('c')).isSymmetric)
  
}