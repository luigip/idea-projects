package odds

import scala.swing._
import scala.swing.event.ButtonClicked


object Temp extends SimpleSwingApplication {

  val messages= new TextArea(22,44){
    editable = false
  }
  
  
  val top = new MainFrame{
    title="testScrollBar"
    contents = new FlowPanel{
      val outputTextScrollPane = new ScrollPane(messages)
      contents += outputTextScrollPane
      contents += new Button("Click me") {
        listenTo(this)
        reactions += {
          case e: ButtonClicked => 
            messages.text += util.Random.nextInt + "\n"
        }
      }
    }
  }
  
  1 :: 2 :: Nil
  
}