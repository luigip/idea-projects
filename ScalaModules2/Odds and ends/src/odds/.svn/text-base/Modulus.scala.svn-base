package odds

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Dimension, Color, Graphics2D}

object Modulus extends SimpleSwingApplication {
  val fac = 0.1
  def top = new MainFrame {
    contents =
      new Panel {
        preferredSize = new Dimension(1000, 800)
        val CHECK_SIZE = 1
        override def paintComponent(g: Graphics2D){
        val(w, h)= (size.width, size.height)
        g.setPaint(Color.WHITE)
        g.fillRect(0,0,w,h)
        g.setPaint(Color.BLACK)
        for {
          x <- 0L until (w, CHECK_SIZE.toInt)
          y <- 0L until (h, CHECK_SIZE.toInt)
          if ((1*math.pow(x*fac, 0.79)* 1* math.pow(y*fac,0.79))
               .toLong /CHECK_SIZE) % 1.5 == 0
        } g.fillRect(x.toInt,y.toInt,CHECK_SIZE, CHECK_SIZE)
        
      }
    }
  }
}