package kuhn

import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.TextColor.RGB
import com.googlecode.lanterna.gui2.Interactable.FocusChangeDirection.{DOWN, UP}
import com.googlecode.lanterna.gui2.Interactable.{FocusChangeDirection, Result}
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.googlecode.lanterna.TerminalPosition
import kuhn.Monad._
import kuhn.Scale._
import kuhn.Song._
import kuhn.ƒ._

import scala.collection.JavaConversions._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

case class Matrix(rows: Array[Array[String]], offset: (Int, Int)) {
  assert(rows.tail.forall(_.size == rows.head.size))
  val height = rows.size
  val width = rows.head.size
  val (offsetX, offsetY) = offset
  val left = offsetX
  val right = left + width
  val top = offsetY
  val bottom = top + height
  def foreach(f: (Int, Int, String) ⇒ Unit): Unit = {
    for {
      (row, rowIndex) ← rows.zipWithIndex
      (cell, colIndex) ← row.zipWithIndex
    } {
      f(colIndex + offsetX, rowIndex + offsetY, cell)
    }
  }
  def apply(x: Int, y: Int): String = rows(y - offsetY)(x - offsetX)
  def copyWithValue(x: Int, y: Int, value: String): Matrix =
    Matrix(rows.updated(y - offsetY, rows(y - offsetY).updated(x - offsetX, value)), offset)
}

object Matrix {
  def apply(asString: String, offset: (Int, Int) = (0, 0)): Matrix = {
    val rows = asString.split("\n").filterNot(_.isEmpty)
    val len = rows.maxBy(_.size).size
    val array = for (row ← rows) yield (for (col ← 0 until len) yield if (col < row.size) row.substring(col, col + 1) else " ").toArray
    new Matrix(array, offset)
  }
}
