package odds

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.lang.Double
import java.awt._
import event.{ActionEvent, ActionListener}
import javax.swing.Timer
import swing.event.MouseClicked

object LinePanel extends SimpleSwingApplication {
  val number = 15
  var quarter = 0

  def top = new MainFrame {
    title = "LinePanel"
    contents = p
  }

  def rotate {
    quarter = (quarter + 1) % 4
  }

  val timer2 = new ScalaTimer(200)

  val p = new Panel { 
    override lazy val peer = new javax.swing.JPanel with SuperMixin {
      override def getMousePosition = {
        val p: Point = super.getMousePosition
        new Point(p.x, getHeight - p.y)
      }
    }
    
    preferredSize = new Dimension(400, 400)
    listenTo(timer2, mouse.clicks)
    reactions += {
      case e: TimerEvent => {
        rotate
        repaint()
      }
      case e: MouseClicked => {
        val oldPoint = e.point
        val newPoint = new Point(e.point.x, size.height - e.point.y)
        println("old: " + oldPoint + " new: " + newPoint)
        val mp: Point = peer.getMousePosition
        println("Mouse position" + mp)
      }
      
    }

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val clipWidth = g.getClipBounds.getWidth.toInt
      val clipHeight = g.getClipBounds.getHeight.toInt
      val dx = (size.width: Double) / number
      val dy = (size.height: Double) / number

      val gs = for {
        i <- 0 until number
        j <- 0 until number
        c = 1 - (i * j toFloat) / number / number
      } yield (g.create( (i * dx).toInt, (j * dy).toInt, clipWidth, clipHeight), c)

      gs foreach {
        case (g, c) => {
          g.setColor(new Color(c, c * 0.5f, c * 0.5f))
          g.fillPolygon(triangles(quarter))
        }
      }
    }

    def triangles(q: Int) = {
      val (x, y) = (size.width / number, size.height / number)
      q match {
        case 0 => new Polygon(Array(0, x, x), Array(0, 0, y), 3)
        case 1 => new Polygon(Array(x, x, 0), Array(0, y, y), 3)
        case 2 => new Polygon(Array(x, 0, 0), Array(y, y, 0), 3)
        case 3 => new Polygon(Array(0, 0, x), Array(y, 0, 0), 3)
      }
    }
  }

  timer2.start
}

//  val timer = new Timer(200, new ActionListener() {
//    def actionPerformed(e: ActionEvent) {
//      rotate
//      p.repaint()
//    }
//  })















