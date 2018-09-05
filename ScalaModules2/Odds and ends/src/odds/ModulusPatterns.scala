package odds

import java.awt.event.{ActionEvent, ActionListener}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Font, Color, Graphics2D, Dimension}
import swing.event.MouseClicked
import math.pow

object ModulusPatterns extends SimpleSwingApplication {
  var delay_ms = 200
//  var factor = 1.0  // zoom factor
  var power = 0.99
  val SIZE = 1      // checker size

  def top = new MainFrame {
    contents = panel
  }

  val panel = new Panel {
    preferredSize = new Dimension(800, 600)

    override def paintComponent(g: Graphics2D) {

      val(w, h) = (size.width, size.height)
      // Blank background
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, w, h)
      // Draw on squares if calculation yields even result
      g.setColor(Color.BLACK)
      for (x <- 0L until (w, SIZE); y <- 0L until (h, SIZE)) {
//        if((x * factor * y).toLong % 2 == 0) {
        if((pow(x,power) * pow(y, power)).toLong % 2 == 0) {
          g.fillRect(x.toInt, y.toInt, SIZE, SIZE)
        }
      }

      // Draw present values box
      val fh = g.getFontMetrics.getHeight
      g.setColor(Color.WHITE)
      g.fillRect(w - 70, h - (2*fh), 70, 2*fh)
      g.setColor(Color.RED)

      g.drawString("p = %.3f".format(power), w - 49, h - (fh + 2))
//      g.drawString("i = %.3f".format(factor), w - 49, h - 2)
    }

    listenTo(mouse.clicks)
    reactions += {
      case e: MouseClicked => {
        val r: Boolean = repainter.isRunning
        if (r) repainter.stop() else repainter.start()
      }
    }
  }

  val repainter: Timer = new Timer(delay_ms, new ActionListener {
    def actionPerformed(e: ActionEvent) {
//      factor += 0.001
      power += 0.0001
      panel.repaint
    }
  })

  repainter.start()
}