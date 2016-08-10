package kuhn

import com.googlecode.lanterna.TextColor.RGB
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import kuhn.Scale._
import ƒ._
import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.gui2.Interactable.{FocusChangeDirection, Result}
import com.googlecode.lanterna.gui2.Interactable.FocusChangeDirection.{DOWN, UP}
import com.googlecode.lanterna.input.KeyStroke

import collection.JavaConversions._

object UI extends App {

  var tracks: List[Track] = List.empty[Track]

  case class Track(
    name: String,
    trackType: String,
    matrix: Matrix,
    rows: List[Row],
    scale: Ratio,
    multilineOffset: (Int, Int)
  )

  case class Row( // metadata that helps us understand a row in the matrix
  )

  val TrackTypeMappingMatrix = "mapping"
  val TrackTypeBeatMatrix = "beat"

}

case class Matrix(rows: Array[Array[Char]], offset: (Int, Int)) {
  assert(rows.tail.forall(_.size == rows.head.size))
  val height = rows.size
  val width = rows.head.size
  val (offsetX, offsetY) = offset
  val left = offsetX
  val right = left + width
  val top = offsetY
  val bottom = top + height
  def foreach(f: (Int, Int, Char) ⇒ Unit): Unit = {
    for {
      (row, rowIndex) ← rows.zipWithIndex
      (cell, colIndex) ← row.zipWithIndex
    } {
      f(colIndex + offsetX, rowIndex + offsetY, cell)
    }
  }
  def apply(x: Int, y: Int): Char = rows(y - offsetY)(x - offsetX)
  def copyWithValue(x: Int, y: Int, value: Char): Matrix =
    Matrix(rows.updated(y - offsetY, rows(y - offsetY).updated(x - offsetX, value)), offset)
}

object Matrix {
  def apply(asString: String, offset: (Int, Int) = (0, 0)): Matrix = {
    val rows = asString.split("\n").filterNot(_.isEmpty)
    val len = rows.maxBy(_.size).size
    val array = for (row ← rows) yield (for (col ← 0 until len) yield if (col < row.size) row.substring(col, col + 1).charAt(0) else ' ').toArray
    new Matrix(array, offset)
  }
}

class MatrixComponent(var matrix: Matrix) extends AbstractInteractableComponent[MatrixComponent] {
  def createDefaultRenderer = new MatrixRenderer

  override def handleKeyStroke(keyStroke: KeyStroke): Result = {
    if (!keyStroke.isAltDown && !keyStroke.isCtrlDown && !keyStroke.isShiftDown) {
      import com.googlecode.lanterna.input.KeyType._
      return keyStroke.getKeyType match {
        case ArrowLeft if cursorPosition.getColumn > 0 ⇒
          cursorPosition = cursorPosition.withRelativeColumn(-1)
          Result.HANDLED
        case ArrowRight if cursorPosition.getColumn < matrix.width - 1 ⇒
          cursorPosition = cursorPosition.withRelativeColumn(+1)
          Result.HANDLED
        case ArrowUp if cursorPosition.getRow > 0 ⇒
          cursorPosition = cursorPosition.withRelativeRow(-1)
          Result.HANDLED
        case ArrowDown if cursorPosition.getRow < matrix.height - 1 ⇒
          cursorPosition = cursorPosition.withRelativeRow(+1)
          Result.HANDLED
        case Character ⇒
          matrix = matrix.copyWithValue(cursorPosition.getColumn, cursorPosition.getRow, keyStroke.getCharacter)
          //          UI.buildSong(matrix)
          Result.HANDLED
        case _ ⇒ super.handleKeyStroke(keyStroke)
      }
    }
    super.handleKeyStroke(keyStroke)
  }


  override def afterEnterFocus(direction: FocusChangeDirection, previouslyInFocus: Interactable): Unit = {
    previouslyInFocus match {
      case p: MatrixComponent if direction == UP || direction == DOWN ⇒ cursorPosition = p.cursorPosition
      case _ ⇒
    }
    super.afterEnterFocus(direction, previouslyInFocus)
  }

  var cursorPosition = TerminalPosition.TOP_LEFT_CORNER
}

object UI0 extends App {

  var m1 = Matrix.apply("""
aaaa
    bbbb
                        """)

  //  val m2 = Matrix.apply("""
  //12345
  //""")
  //
  //  val m3 = Matrix.apply("""
  //23456
  //""")

  val terminal = new DefaultTerminalFactory().setForceTextTerminal(true).createTerminal()
  val screen = new TerminalScreen(terminal)
  screen.startScreen
  val gui = new MultiWindowTextGUI(screen, new DefaultWindowManager(), new EmptySpace(new RGB(40, 10, 10)))
  val window = new BasicWindow
  window.setHints(List(Window.Hint.FULL_SCREEN, Window.Hint.NO_DECORATIONS))
  window.setCloseWindowWithEscape(true)
  val panel = new Panel(new LinearLayout(Direction.VERTICAL))
  panel.addComponent(new MatrixComponent(m1))
  //  panel.addComponent(new MatrixComponent(m2))
  //  panel.addComponent(new MatrixComponent(m3))
  window.setComponent(panel)//.withBorder(Borders.doubleLineBevel))
  //  window.setHints(List(Window.Hint.EXPANDED))


  def buildSong(m: Matrix): Unit = {
    MatrixNoteGenerator(m) >>
      //    MatrixNoteGenerator(m2) >>
      //    MatrixNoteGenerator(m3) >>
      NoteOfScalesToNotes(MajorScale(C)) >> MasterOut apply NoteSequence.empty
  }

  buildSong(m1)


  gui.addWindowAndWait(window)
  R.close
}

