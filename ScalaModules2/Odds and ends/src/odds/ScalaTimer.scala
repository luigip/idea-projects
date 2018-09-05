package odds

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer
import swing.Publisher

case class TimerEvent (source: AnyRef) extends swing.event.Event

/**
 * Scala Timer by wrapping Java Timer
 */
class ScalaTimer(delayTime: Int) extends Publisher {
  outer =>

  private val t = new Timer(delayTime, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      publish(new TimerEvent(outer))
    }
  })

  def addActionListener(listener: ActionListener) {t.addActionListener(listener)}
  def fireActionPerformed(e: swing.event.ActionEvent) {publish(e)}
  def actionCommand: String = t.getActionCommand
  def actionListeners: Array[ActionListener] = t.getActionListeners
  def delay: Int = t.getDelay
  def initialDelay = t.getInitialDelay
  def listeners[A <: java.util.EventListener](listenerType: Class[A]) = t.getListeners(listenerType)
  def coalesce: Boolean = t.isCoalesce
  def repeats: Boolean = t.isRepeats
  def running: Boolean = t.isRunning
  def removeActionListener(listener: ActionListener) {t.removeActionListener(listener)}
  def restart() {t.restart()}
  def actionCommand_=(command: String) {t.setActionCommand(command)}
  def coalesce_=(flag: Boolean) {t.setCoalesce(flag)}
  def delay_=(delay: Int) {t.setDelay(delay)}
  def initialDelay_=(initialDelay: Int) {t.setInitialDelay(initialDelay)}
  def repeats_=(flag: Boolean) {t.setRepeats(flag)}
  def start() {t.start()}
  def stop() {t.stop()}
}

object ScalaTimer {
  def getLogTimers: Boolean = Timer.getLogTimers
  def setLogTimers(flag: Boolean) {Timer.setLogTimers(flag)}
}

/**
 * Scala Timer by extending Java Timer
 */
class ScalaTimer2 (delay: Int) extends Timer(delay, null) with Publisher {
  outer =>
  addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      publish(new TimerEvent(outer))
    }
  })
}

object ScalaTimer2 {
  def getLogTimers: Boolean = Timer.getLogTimers
  def setLogTimers(flag: Boolean) {Timer.setLogTimers(flag)}
}

/**
 * Using companion object's apply method
 */
class ScalaTimer3 private (delay: Int, listener: ActionListener)
      extends Timer(delay, listener) with Publisher {
}

object ScalaTimer3 {
  def apply(delay: Int): ScalaTimer3 = {
    lazy val ret: ScalaTimer3 = new ScalaTimer3(delay, new ActionListener {
      def actionPerformed(e: ActionEvent) {
        ret.publish(TimerEvent(ret))
      }
    })
    ret
  }
}