package odds

/**
 * Created by Rhys on 06/08/2015.
 *
 * Using crummy mutable Java state on purpose
 *
 */
object InvertTreeJava extends App {

  class TreeNode(var value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
    override def toString = s"[$value]-L${str(left)}-R${str(right)}"
    private def str(t: TreeNode): String = Option(t).fold(".")(_.toString)
  }

  val test: TreeNode = new TreeNode(4) {
    left = new TreeNode(2) {
      left = new TreeNode(1)
      right = new TreeNode(3)
    }
    right = new TreeNode(7) {
      left = new TreeNode(6)
      right = new TreeNode(9)
    }
  }

  println(test.toString)

  def invertTree(root: TreeNode): Unit = {
    import root._
    if (left == null || right == null) return
    else {
      invertTree(left)
      invertTree(right)
      val temp = left
      left = right
      right = temp
    }
  }

  invertTree(test)

  println(test.toString)
}

