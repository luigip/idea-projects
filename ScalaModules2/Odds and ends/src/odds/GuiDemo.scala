package odds


import swing._
import event.ButtonClicked
import scala.concurrent._
import duration._
//import ExecutionContext.Implicits.global

/*
Returning value to main method, for this question:
http://stackoverflow.com/questions/17282622/return-a-value-from-a-scala-swing-app/17374346#17374346
 */

object GuiDemo extends App {

  object GUI extends SimpleSwingApplication {
    val button = new Button {
      text = "Go!"
    }
    
    val comboBox = new ComboBox(List("a", "b", "c"))
    def top = new MainFrame {
      contents = new FlowPanel {
        contents += comboBox
        contents += button
      }

      listenTo(button)

      reactions += {
        case ButtonClicked(`button`) => {
          val selection = comboBox.item
          button.enabled_= (false)
        }
      }
    }
  }

  val p = promise[String]
  
  new Reactor {
    listenTo(GUI.button)
    reactions += {
      case e: ButtonClicked => p.success(GUI.comboBox.item)
    }
  }

  println("Before starting the GUI")
  GUI.main(args)
  
  val myValue = Await.result(p.future, Duration.Inf)
  println("Now i need to make a complicated transformation in scala with the selected value: " + myValue.map(_.toUpper) ) // how do i get the selected value from the GUI?


}