package kuhn

import java.lang.Math.{max, min}
import javax.sound.midi.MidiSystem

import com.googlecode.lanterna.{SGR, TextColor}
import com.googlecode.lanterna.graphics.SimpleTheme
import com.googlecode.lanterna.gui2.Interactable.Result
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.input.KeyType._
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import kuhn.ƒ._

import scala.collection.JavaConversions._
import scala.collection._

object UI extends App {
  val m1 = new MatrixComponent(changeHandler)
  System.setProperty("apple.awt.UIElement", "true")
  val terminal = new DefaultTerminalFactory().createTerminal()
  val screen = new TerminalScreen(terminal)
  screen.startScreen()
  val gui = new MultiWindowTextGUI(screen, new DefaultWindowManager(), new EmptySpace)
  val window = new BasicWindow {
    override def handleInput(key: KeyStroke): Boolean = {
      if (key.getKeyType == KeyType.F1) {
        true
      } else {
        super.handleInput(key)
      }
    }
  }
  window.setHints(List(Window.Hint.FULL_SCREEN, Window.Hint.NO_DECORATIONS))
  object Theme extends SimpleTheme(MatrixRenderer.FG, MatrixRenderer.BG) {
    getDefaultDefinition.setCustom("value", new TextColor.RGB(140, 140, 140), MatrixRenderer.BG)
    getDefaultDefinition.setCustom("value_negative", new TextColor.RGB(140, 140, 140), MatrixRenderer.BG, SGR.UNDERLINE)
    getDefaultDefinition.setCustom("hidden_but_visible", new TextColor.RGB(100, 100, 100), MatrixRenderer.BG)
  }
  window.setTheme(Theme)
  val layoutManager = new LinearLayout
  val panel = new Panel(layoutManager)
  window.setComponent(panel)
  val pos = new Label("???")
  var midiReceiver: MidiReceiver = new MidiReceiver({
    i ⇒
      pos.setText(i.toString)
      m1.transportLocation = i
  })
  MidiSystem.getTransmitter.setReceiver(midiReceiver)
  def changeHandler(matrixComponent: MatrixComponent) {
    new MasterOut(midiReceiver)(matrixComponent.values)
  }
  panel.addComponent(m1)
  panel.addComponent(pos)
  m1.takeFocus
  gui.addWindowAndWait(window)
}

class MatrixComponent(change: MatrixComponent ⇒ Unit) extends AbstractInteractableComponent[MatrixComponent] {

  var name = "my 1st matrix"
  var width = 16*beats
  var gridX = 1*beats
  var showHidden = false
  var transportLocation = 0

  case class Val(value: Char, negative: Boolean = false) {
    val toInt = value.asDigit * (if (negative) -1 else 1)
    def invert = copy(negative = !negative)
  }

  case class Row(
    name: String,
    values: List[Val] = List.empty,
    visible: Boolean = true
  ) {
    val paddedValues: List[Val] = values.padTo(width, Val(' '))
    def valAt(x: Int): Val = paddedValues(x)
    def optValAt(x: Int): Option[Val] = valAt(x) match {
      case Val(' ', _) ⇒ None
      case v ⇒ Some(v)
    }
    def optIntValAt(x: Int) = optValAt(x).map(_.toInt)
    def copyWithValue(index: Int, value: Val) = copy(values = paddedValues.updated(index, value))
  }

  var rows = List(
    Row("duration", visible = false),
    Row("value"),
    Row("octave", visible = false),
    Row("accidental", visible = false),
    Row("attack", visible = false),
    Row("attack_fine", visible = false),
    Row("release", visible = false),
    Row("release_fine", visible = false)
  )

  def rowNameMaxSize = rows.filter(_.visible || showHidden).map(_.name.size).max
  def rowAt(y: Int) = rows(y)
  def valAt(x: Int, y: Int) = rows(y).valAt(x)
  def setRowVisible(y: Int, visible: Boolean) { rows = rows.updated(y, rows(y).copy(visible = visible)) }
  def updateValAt(x: Int, y: Int, v: Val) { rows = rows.updated(y, rows(y).copyWithValue(x, v)) }
  def rowMap = (for (r ← rows) yield r.name → r).toMap
  def createDefaultRenderer = new MatrixRenderer

  override def handleKeyStroke(keyStroke: KeyStroke): Result = {
    val result = keyStroke.getKeyType match {
      case ArrowLeft ⇒
        if (keyStroke.isAltDown) colViewOffset -= 1 * beats
        else activeColIndex -= (if (keyStroke.isShiftDown) 1 * beats else 1)
        Result.HANDLED
      case ArrowRight ⇒
        if (keyStroke.isAltDown) colViewOffset += 1 * beats
        else activeColIndex += (if (keyStroke.isShiftDown) 1 * beats else 1)
        Result.HANDLED
      case ArrowUp if activeRowIndex > 0 ⇒
        activeRowIndex -= 1
        Result.HANDLED
      case ArrowDown if activeRowIndex < rows.size - 1 ⇒
        activeRowIndex += 1
        Result.HANDLED
      case Character if activeRowIsVisible ⇒
        keyStroke.getCharacter.toChar match {
          case 'a' if keyStroke.isCtrlDown ⇒
            showHidden = !showHidden
          case 's' if keyStroke.isCtrlDown ⇒
            setRowVisible(activeRowIndex, !rowAt(activeRowIndex).visible)
          case '-' ⇒
            updateValAt(activeColIndex, activeRowIndex, valAt(activeColIndex, activeRowIndex).invert)
            change(this)
            activeColIndex += 1
          case _ ⇒
            updateValAt(activeColIndex, activeRowIndex, Val(keyStroke.getCharacter))
            change(this)
            activeColIndex += 1
        }
        Result.HANDLED
      case Backspace if activeColIndex > 0 ⇒
        activeColIndex -= 1
        updateValAt(activeColIndex, activeRowIndex, Val(' '))
        change(this)
        Result.HANDLED
      case _ ⇒ super.handleKeyStroke(keyStroke)
    }

    activeColIndex = max(activeColIndex, 0)
    activeColIndex = min(activeColIndex, width - 1)
    result
  }

  var activeRowIndex = 1
  def activeRowIsVisible = rowAt(activeRowIndex).visible || showHidden
  def activeVisibleRowIndex = (0 until activeRowIndex).map(rowAt).count(_.visible || showHidden)
  var activeColIndex = 0
  var colViewOffset = 0
  def colViewWidth(graphics: TextGUIGraphics) = graphics.getSize.getColumns - rowNameMaxSize
  def numVisibleRows = rows.count(_.visible || showHidden)

  def values: Sequence = {
    val values = new mutable.ListBuffer[Value]
    val map = rowMap
    var d = 1
    var o = 0
    var attack = 10
    var attackFine = 0
    var release = 10
    var releaseFine = 0
    for (x ← 0 until width) {
      map("duration").optIntValAt(x).foreach(d = _)
      val value = map("value").optIntValAt(x)
      map("octave").optIntValAt(x).foreach(o = _)
      val a = map("accidental").optIntValAt(x).getOrElse(0)
      map("attack").optIntValAt(x).foreach(attack = _)
      map("attack_fine").optIntValAt(x).foreach(attackFine = _)
      map("release").optIntValAt(x).foreach(release = _)
      map("release_fine").optIntValAt(x).foreach(releaseFine = _)
      value match {
        case Some(v) ⇒ values += Value(x, d, v, o, a, attack, attackFine, release, releaseFine)
        case _ ⇒
      }
    }
    Sequence(values, width)
  }
}
