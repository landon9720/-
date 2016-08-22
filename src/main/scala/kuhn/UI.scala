package kuhn

import java.io.Serializable
import java.lang.Math.{max, min}
import javax.sound.midi.MidiSystem
import javax.swing.JFrame

import com.googlecode.lanterna.screen.Screen.RefreshType
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.{TerminalSize, SGR, TextColor}
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

  val scaleMatrix = new Matrix("scale", List(Row(Left("value"), "101011010101")), changeHandler, showTransportControl = false, scale = (1, 1), multilineValueValue = "")

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
  val degreeOfScale = new Matrix("degree of scale", top, changeHandler, multilineValueValue = "duration")

  val chordMatrix = new Matrix("chord", List(Row(Left("value"), "1010100")), changeHandler, showTransportControl = false, scale = (1, 1), multilineValueValue = "")

  val top2: List[Row] = List(
    Row(Left("duration")),
    Row(Left("value")),
    Row(Left("octave")),
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
    Row(Left("accidental")),
    Row(Left("attack")),
    Row(Left("release"))
  )
  val degreeOfChord = new Matrix("degree of chord", top2, changeHandler, multilineValueValue = "duration")

  val matrixies = List(scaleMatrix, degreeOfScale, chordMatrix, degreeOfChord)

  System.setProperty("apple.awt.UIElement", "true")
  val factory = new DefaultTerminalFactory()
  val terminal = factory.createSwingTerminal()
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
  window.setHints(List(Window.Hint.NO_DECORATIONS, Window.Hint.NO_POST_RENDERING))
//  window.setSize(terminal.getTerminalSize.withRows(300))
  object Theme extends SimpleTheme(FG, BG) {
    getDefaultDefinition.setCustom("value", FG_VALUE, BG)
    getDefaultDefinition.setCustom("value_negative", FG_VALUE, BG, SGR.UNDERLINE)
  }
  window.setTheme(Theme)
  val layoutManager = new LinearLayout
  layoutManager.setSpacing(1)
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
    val stack = (
      (StartHere(degreeOfScale.asValues) >> scaleMatrix.asScale) ===
      (StartHere(degreeOfChord.asValues) >> chordMatrix.asScale >> scaleMatrix.asScale)
    ) >> new MasterOut(midiReceiver)
    stack(Sequence.empty)
  }
  matrixies.foreach(panel.addComponent)
  panel.addComponent(pos)
//  panel.invalidate()
//  screen.refresh(RefreshType.COMPLETE)
  scaleMatrix.takeFocus
//  screen.doResizeIfNecessary()
//  window.invalidate()
  terminal.setVisible(true)
  terminal.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  terminal.setTitle("ƒ")
//  screen.refresh()
  gui.addWindowAndWait(window)
}

