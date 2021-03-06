package odds

import java.awt.event.{ActionEvent, ActionListener}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Font, Color, Graphics2D, Dimension}
import java.awt.image.BufferedImage
import util.Random

object ImageNoise2 extends SimpleSwingApplication {
  var framecount = 0
  var fps = 0
  var buffer: BufferedImage = _

  def top = new MainFrame {
    contents = panel
  }

  val panel = new Panel {
    preferredSize = new Dimension(320, 240)

    override def paintComponent(g: Graphics2D) {
      drawNoise()
      g.drawImage(buffer, 0, 0, null)
      g.setColor(Color.RED)
      g.setFont(new Font("Monospaced", Font.BOLD, 20))
      g.drawString("FPS: " + fps, size.width - 100, size.height - 10)
      framecount += 1
    }
  }
  
  def drawNoise() {
    val (w,h) = (panel.size.width, panel.size.height)
    if(null == buffer || buffer.getWidth != w || buffer.getHeight != h){
          buffer = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_BINARY);
    }
    val wr = buffer.getRaster
    val rand = new Random
    for (x <- 0 until w; y <- 0 until h) {
      wr.setSample(x, y, 0, (rand.nextFloat + 0.5).toInt)
    }
  }
  
  val repainter = new Timer(2, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      panel.repaint
    }
  })

  val frameratechecker = new Timer(1000, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      fps = framecount
      framecount = 0
    }
  })

  repainter.start()
  frameratechecker.start()
}