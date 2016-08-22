package kuhn

import java.lang.Math.{max, min}
import javax.sound.midi.MidiSystem

import com.googlecode.lanterna.{SGR, TextColor}
import com.googlecode.lanterna.graphics.SimpleTheme
import com.googlecode.lanterna.gui2.Interactable.{FocusChangeDirection, Result}
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.input.KeyType._
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import kuhn.ƒ._
import kuhn.Monad._

import scala.Predef
import scala.collection.JavaConversions._
import scala.collection._



case class Val(value: Char, negative: Boolean = false) {
  val toInt = value.asDigit * (if (negative) -1 else 1)
  def withValue(value: Char) = copy(value = value)
  def invert = copy(negative = !negative)
}

object Val {
  val Empty = Val(' ')
}

case class Row(
  key: Either[String, Val],
  values: Map[Int, Val] = Map.empty
  ) {
  val name = key match {
    case Left(name) ⇒ name
    case Right(Val(v, neg)) ⇒ s"${if (neg) "-" else ""}$v"
  }
  def valAt(x: Int): Val = values.getOrElse(x, Val.Empty)
  def optValAt(x: Int): Option[Val] = valAt(x) match {
    case Val(' ', _) ⇒ None
    case v ⇒ Some(v)
  }
  def optIntValAt(x: Int): Option[Int] = optValAt(x).map(_.toInt)
  def copyWithValue(index: Int, value: Val): Row = copy(values = values + (index → value))
  def clear: Row = copy(values = Map.empty)
  def toIterator = new scala.collection.Iterator[Val] {
    private var i = 0
    def hasNext = true
    def next = {
      val v = valAt(i)
      i += 1
      v
    }
  }
}

object Row {
  def apply(key: Either[String, Val]): Row = apply(key, "")
  def apply(key: Either[String, Val], string: String): Row =
    string.zipWithIndex.foldLeft(new Row(key)) {
      case (a: Row, (v: Char, i: Int)) ⇒
        a.copyWithValue(i, Val(v))
    }
}

class Matrix(val name: String, var rows: List[Row], change: Matrix ⇒ Unit, val showTransportControl: Boolean = true, var scale: Ratio = (4, 1), multilineValueValue: String) extends AbstractInteractableComponent[Matrix] {

  var gridX = 1*beats
  var transportLocation = 0
  def transportLocationScaled = transportLocation productWithRatio scale.invert

  def rowNameMaxSize = rows.map(_.name.size).max
  def rowAt(y: Int) = rows(y)
  def valAt(x: Int, y: Int) = rows(y).valAt(x)
  def updateValAt(x: Int, y: Int, v: Val) { rows = rows.updated(y, rows(y).copyWithValue(x, v)) }
  def clearRow(y: Int) { rows = rows.updated(y, rowAt(y).clear) }
  def rowMap = (for (r ← rows) yield r.name → r).toMap.withDefault(k ⇒ Row(Left(k)))
  def multilineValueMap: Map[Val, Row] = (for (r ← rows if r.key.isRight) yield r.key.right.get → r).toMap

  def createDefaultRenderer = new MatrixRenderer

  override def handleKeyStroke(keyStroke: KeyStroke): Result = {
    val result = keyStroke.getKeyType match {
      case ArrowLeft ⇒
//        if (keyStroke.isAltDown) colViewOffset -= 1 * beats
//        else
    activeColIndex -= (if (keyStroke.isShiftDown) 1 * beats else 1)
        Result.HANDLED
      case ArrowRight ⇒
//        if (keyStroke.isAltDown) colViewOffset += 1 * beats
//        else
    activeColIndex += (if (keyStroke.isShiftDown) 1 * beats else 1)
        Result.HANDLED
      case ArrowUp if activeRowIndex > 0 ⇒
        activeRowIndex -= 1
        Result.HANDLED
      case ArrowDown if activeRowIndex < rows.size - 1 ⇒
        activeRowIndex += 1
        Result.HANDLED
      case Character ⇒
        keyStroke.getCharacter.toChar match {
          case '-' ⇒
            updateValAt(activeColIndex, activeRowIndex, valAt(activeColIndex, activeRowIndex).invert)
            change(this)
            activeColIndex += 1
          case _ ⇒
            updateValAt(activeColIndex, activeRowIndex, valAt(activeColIndex, activeRowIndex).withValue(keyStroke.getCharacter))
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
    result
  }

  var activeRowIndex = 0
  var activeColIndex = 0
  var colViewOffset = 0

  def asValues: Sequence = {
    val values = new mutable.ListBuffer[Value]
    val map = rowMap
    var d = 1 productWithRatio scale
    var o = 0
    var attack = 10
    var attackFine = 0
    var release = 10
    var releaseFine = 0
    val first = 0 // TODO
    val last = 12*beats // TODO
    for (x ← first to (last+1)) {
      val time = x productWithRatio scale
      map("duration").optIntValAt(x).foreach(d0 ⇒ d = d0 productWithRatio scale)
      val value = map("value").optIntValAt(x)
      map("octave").optIntValAt(x).foreach(o = _)
      var a = map("accidental").optIntValAt(x).getOrElse(0)
      map("attack").optIntValAt(x).foreach(attack = _)
      map("attack_fine").optIntValAt(x).foreach(attackFine = _)
      map("release").optIntValAt(x).foreach(release = _)
      map("release_fine").optIntValAt(x).foreach(releaseFine = _)
      value match {
        case Some(v) ⇒ values += Value(time, d, v, o, a, attack, attackFine, release, releaseFine)
        case _ ⇒
      }
      for ((keyValue, row) ← multilineValueMap) {
        val secondaryValue = row.optIntValAt(x)
        secondaryValue match {
          case Some(v) ⇒
            multilineValueValue match {
              case "duration" ⇒ d = v
              case "octave" ⇒ o = v
              case "accidental" ⇒ a = v
              case "attack" ⇒ attack = v
              case "attack_fine" ⇒ attackFine = v
              case "release" ⇒ release = v
              case "release_fine" ⇒ releaseFine = v
            }
            values += Value(time, d, keyValue.toInt, o, a, attack, attackFine, release, releaseFine)
          case _ ⇒
        }
      }
    }
    Sequence(values)
  }
  
  def asScale: Monad = {
    val binary = rowMap("value").toIterator.takeWhile(v ⇒ v.value == '0' || v.value == '1')
    val ranks = binary.zipWithIndex.filter(_._1.value == '1').map(_._2).toList // indexes of 1's
    if (ranks.nonEmpty) {
      RankScale(ranks)
    } else {
      say(s"Ok, I do not understand the matrix $name")
      new Scale {
        def apply(input: Sequence): Sequence = Sequence.empty
      }
    }
  }

  override def afterEnterFocus(direction: FocusChangeDirection, previouslyInFocus: Interactable): Unit = {
    previouslyInFocus match {
      case m: Matrix ⇒ activeColIndex = m.activeColIndex
      case _ ⇒
    }
    super.afterEnterFocus(direction, previouslyInFocus)
  }
}

