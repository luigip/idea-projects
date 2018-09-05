package odds

import scala.swing._
import scala.swing.Swing._
import java.awt.{Color, RenderingHints, BasicStroke, Point}
import java.util.{GregorianCalendar, TimerTask, Calendar}
import java.awt.geom.{Point2D, Line2D}

object ClockGUI extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Clock"
    contents = new ClockPanel
  }
}

class ClockPanel extends Panel {
  
  var clock = new Clock
  val minHandLength = 0.85
  val hourHandLength = 0.5
  val secondHandLength = 0.75
  val pad = 5  
  preferredSize = (400, 400)

  def polarToCartesian(angle: Double, length: Double, centre: Point) = new Point2D.Double (
    (centre.x + length * (size.width  - 2*pad)/2 * math.sin(angle * math.Pi / 180)), 
    (centre.y - length * (size.height - 2*pad)/2 * math.cos(angle * math.Pi / 180))
  )
  
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    
    val centre = new Point(size.width/2, size.height/2)
    // face
    g.setStroke(new BasicStroke(5))
    g.drawArc(0 + pad, 0 + pad, size.width - 2*pad, size.height - 2*pad, 0, 360)

    // hands
    def drawHand(angle: Double, length: Double, centre: Point, thickness: Float, colour: Color) {
      val pt = polarToCartesian(angle, length, centre)
      g.setStroke(new BasicStroke(thickness))
      g.setColor(colour)
      val line = new Line2D.Double(centre.x, centre.y, pt.x, pt.y)
      g.draw(line)
    }
    
    // render end points at sub-pixel level
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    drawHand(clock.hourAngle, hourHandLength, centre, 3, Color.black)
    drawHand(clock.minuteAngle, minHandLength, centre, 2, Color.blue)
    drawHand(clock.secondAngle, secondHandLength, centre, 0.5f, Color.red)
  }
  
  val timer = new java.util.Timer()
  timer.schedule(new TimerTask {
    def run() {
      clock = new Clock  
      repaint()
    }
  }, 1000, 1000)
  
}

class Clock {
  import Calendar._
  val cal = new GregorianCalendar()
  val secondAngle = cal.get(SECOND) * 6.0
  val minuteAngle = cal.get(MINUTE) * 6.0  + secondAngle / 60
  val hourAngle   = cal.get(HOUR)   * 30.0 + minuteAngle / 12
}