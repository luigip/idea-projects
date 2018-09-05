package odds

import swing._
import event.MouseClicked
import Swing._
import java.awt.Color
import odds.Life.Point

object Display extends SimpleSwingApplication {

  def top = new MainFrame {
    contents = new LifePanel(10, 10, Life.grid((2, 1), (3, 1), (4, 1)), 2000)
  }

}

class LifePanel(val xPoints: Int, val yPoints: Int, var points: Set[Point], delay: Int) extends Panel {

  background = Color.white
  preferredSize = (400, 400)
  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked =>
      togglePoint(e.point.x, e.point.y)
      repaint()
  }

  def togglePoint(x: Int, y: Int) {
    val x1 = x / dx
    val y1 = y / dy
    val p = Point.get(x1,y1)
    if (points(p)) points -= p else points += p
  }

  def dx = this.size.width / xPoints
  def dy = this.size.height / yPoints

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)

    for {
      x <- 0 until xPoints
      y <- 0 until yPoints
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      if (points(Point.get(x, y)))
        g.setColor(Color.blue)
        else g.setColor(Color.WHITE)
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }

  val timer = new javax.swing.Timer (delay, ActionListener{e =>
    points = Life.next(points)
    repaint()
  })

  timer.start()

}