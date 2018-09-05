package odds

/**
 * Created by Rhys on 06/08/2015.
 */
object InvertTreeScala extends App {
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])

  def T(value: Int) = TreeNode(value, None, None)
  def T(value: Int, left: TreeNode, right: TreeNode) = TreeNode(value, Some(left), Some(right))
  val test = T(4,T(2,T(1),T(3)),
                 T(7,T(6),T(9)))

  def invertTree(root: TreeNode): TreeNode = root match {
      case TreeNode(_,None,None) => root
      case TreeNode(value, Some(left), Some(right)) => T(value, invertTree(right), invertTree(left))
  }

  println(test)
  println(invertTree(test))

}

