package odds

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}

object Draw extends SimpleSwingApplication {

  val data = Array.ofDim[Color](50, 50)
  
  // plot some points
  data(0)(0) = Color.BLACK
  data(4)(4) = Color.RED
  data(0)(4) = Color.GREEN
  data(4)(0) = Color.BLUE

  // draw a circle
  import math._
  {
    for {
      t <- Range.Double(0, 2 * Pi, Pi / 100)
      r <- 0 to 20
      x = 25.5 + r * cos(t)
      y = 25.5 + r * sin(t)
      c = Color.getHSBColor((t / 2 / Pi).toFloat,  1f,  1f)
    } data(x.toInt)(y.toInt) = c
  }

  def top = new MainFrame {
    contents = new DataPanel(data) {
      preferredSize = new Dimension(300, 300)
    }
  }
}

class DataPanel(data: Array[Array[Color]]) extends Panel {

  override def paintComponent(g: Graphics2D) {

    val dx = g.getClipBounds.width.toFloat  / data.length
    val dy = g.getClipBounds.height.toFloat / data.map(_.length).max

    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.WHITE)
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}