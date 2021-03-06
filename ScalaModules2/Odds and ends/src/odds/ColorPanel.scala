package odds

import swing._
import java.awt.Color

object ColorPanel extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Flash!"
    contents = p
  }

  val timer = new ScalaTimer(100)

  val p = new Panel {
    var c: Color = new Color(0)
    preferredSize = new Dimension(200, 200)
    listenTo(timer)
    reactions += {
      case e: TimerEvent => {
        c = new Color((c.getRGB + 1000) % 16777216)
        repaint()
      }
    }

    override def paintComponent(g: Graphics2D) {
      g.setColor(c)
      g.fillRect(0, 0, size.width, size.height)
    }
  }

  timer.start()
}





