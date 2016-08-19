package kuhn

import java.io.Serializable
import java.lang.Math.{max, min}
import javax.sound.midi.MidiSystem

import com.googlecode.lanterna.{SGR, TextColor}
import com.googlecode.lanterna.graphics.SimpleTheme
import com.googlecode.lanterna.gui2.Interactable.{FocusChangeDirection, Result}
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.input.KeyType._
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.screen.{VirtualScreen, TerminalScreen}
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import kuhn.Colors._
import kuhn.ƒ._
import kuhn.Monad._

import scala.collection.JavaConversions._
import scala.collection._

object UI extends App {

  val m0 = new Matrix("scale", List(Row(Left("value"), "101011010101")), changeHandler, showTransportControl = false, scale = (1, 1), multilineValueValue = "")

  val top: List[Row] = List(
    Row(Left("duration")),
    Row(Left("value")),
    Row(Left("octave")),
    Row(Right(Val('9', negative = false))),
    Row(Right(Val('8', negative = false))),
    Row(Right(Val('7', negative = false))),
    Row(Right(Val('6', negative = false))),
    Row(Right(Val('5', negative = false))),
    Row(Right(Val('4', negative = false))),
    Row(Right(Val('3', negative = false))),
    Row(Right(Val('2', negative = false))),
    Row(Right(Val('1', negative = false))),
    Row(Right(Val('0', negative = false))),
    Row(Right(Val('1', negative = true))),
    Row(Right(Val('2', negative = true))),
    Row(Right(Val('3', negative = true))),
    Row(Right(Val('4', negative = true))),
    Row(Right(Val('5', negative = true))),
    Row(Right(Val('6', negative = true))),
    Row(Right(Val('7', negative = true))),
    Row(Right(Val('8', negative = true))),
    Row(Right(Val('9', negative = true))),
    Row(Left("accidental")),
    Row(Left("attack")),
    Row(Left("release"))
  )
  val m1 = new Matrix("degree_of_scale", top, changeHandler, multilineValueValue = "duration")
  val m2 = new Matrix("m2", top, changeHandler, multilineValueValue = "attack")

  val matrixies = List(m0, m1, m2)

  System.setProperty("apple.awt.UIElement", "true")
  val terminal = new DefaultTerminalFactory().createTerminal()
  val screen = new TerminalScreen(terminal)
  screen.startScreen()
  val gui = new MultiWindowTextGUI(screen, new DefaultWindowManager(), new EmptySpace)
  val window = new BasicWindow {
    override def handleInput(key: KeyStroke): Boolean = {
      if (key.getKeyType == KeyType.F1) {
        val menu = new BasicWindow("Menu")
        menu.setHints(List(Window.Hint.EXPANDED))
        menu.setCloseWindowWithEscape(true)
        gui.addWindowAndWait(menu)
        true
      } else {
        super.handleInput(key)
      }
    }
  }
  window.setHints(List(Window.Hint.FULL_SCREEN, Window.Hint.NO_DECORATIONS))
  object Theme extends SimpleTheme(FG, BG) {
    getDefaultDefinition.setCustom("value", FG_VALUE, BG)
    getDefaultDefinition.setCustom("value_negative", FG_VALUE, BG, SGR.UNDERLINE)
  }
  window.setTheme(Theme)
  val layoutManager = new LinearLayout
  val panel = new Panel(layoutManager)
  window.setComponent(panel)
  val pos = new Label("")
  var midiReceiver: MidiReceiver = new MidiReceiver({
    i ⇒
      pos.setText(i.toString)
      matrixies.foreach(_.transportLocation = i)
  })
  MidiSystem.getTransmitter.setReceiver(midiReceiver)
  def changeHandler(matrixComponent: Matrix) {
    val stack = ((StartHere(m1.asValues) >> m0.asScale) === StartHere(m2.asValues)) >> new MasterOut(midiReceiver)
    stack(Sequence.empty)
  }
  matrixies.foreach(panel.addComponent)
  panel.addComponent(pos)
  m1.takeFocus
  gui.addWindowAndWait(window)
}

