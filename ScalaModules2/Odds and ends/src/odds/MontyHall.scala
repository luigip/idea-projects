package odds

import scala.swing._
import Swing._
import BorderPanel.Position._
import java.awt.{Font, Color}
import scala.swing.event.ActionEvent
import scala.util.Random


object MontyHall extends SimpleSwingApplication {

  def top = mainFrame

  val timer = new ScalaTimer2(1)
  timer.start()

  val mainFrame = new MainFrame {
       
    val panel = new BorderPanel {
      var switchCount = 0
      var stickCount = 0
      var total = 0
      
      preferredSize = (600, 400)
      
      val stickPanel = new GraphPanel("Stick") with MontyPlayer { val switch = false }
      val switchPanel = new GraphPanel("Switch") with MontyPlayer { val switch = true }
      val goButton = new Button("Go!")      
      val trials = new Label("Trials: 0")      

      layout += stickPanel -> West
      layout += trials -> Center
      layout += switchPanel -> East
      layout += goButton -> South

      listenTo(timer, goButton)
      reactions += {
        case ActionEvent(e) => 
          if (timer.isRunning) timer.stop() else timer.start()
        
        case TimerEvent(e) => {
          if (switchPanel.result) switchCount += 1
          if (stickPanel.result) stickCount += 1
          total += 1
          trials.text = "Trials: " + total
          switchPanel.percent = switchCount / total.toDouble
          stickPanel.percent = stickCount / total.toDouble
          switchPanel.repaint()
          stickPanel.repaint()
        }
      }
    }

    title = "Monty Hall"
    contents = panel
    
  }
   
  
  class GraphPanel(title: String) extends Panel {
    var percent = 0.0
    
    preferredSize = (200, 200)
    
    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      val X = size.width
      val Y = size.height
      val x0 = X/5
      val y0 = Y/5
      val fillWidth  =  (X - 2 * x0 - 1)
      val fillHeight = ((Y - 2 * y0 - 1) * percent).toInt
      g.setColor(Color.black)
      g.setFont(new Font("Consolas", Font.BOLD, 20))
      g.drawString(title + ": " + "%.1f".format(percent*100) + " %", 10, 30)
      g.drawRect(x0,y0,X-2*x0,Y-2*y0)
      g.setColor(Color.blue)
      g.fillRect(x0 + 1, Y - y0 - fillHeight, fillWidth, fillHeight)

    }
    
  }
  
  trait MontyPlayer {
    val switch: Boolean
    def result: Boolean = {
      val prize = Random.nextInt(3)
      val choice = Random.nextInt(3)
      val remaining = Set(0,1,2) - choice - prize
      val open = remaining.toArray match {
        case Array(x) => x
        case Array(x, y) => if (Random.nextBoolean()) x else y
      }
      if (switch)
        (Set(0,1,2) - choice - open).head == prize
      else
        choice == prize
    } 
    
  }
  
}
