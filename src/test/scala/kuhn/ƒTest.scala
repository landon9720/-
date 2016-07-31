package kuhn

import org.junit.Test
import org.junit.Assert._
import Scale._
import ƒ._

class ƒTest {
  @Test
  def test1 = {
    val c = Scale("WWHWWW", C)
    println("C")
    println((-7 to 7) map { NoteOfScale(0, _) } map c.apply mkString "\n")
    val d = Scale("WWHWWW", D)
    println("D")
    println((0 to 7) map { NoteOfScale(0, _) } map d.apply mkString "\n")
    val e = Scale("WWHWWW", E)
    println("E")
    println((0 to 7) map { NoteOfScale(0, _) } map e.apply mkString "\n")
  }

  @Test
  def productWithRatioTest = {
    assertEquals(0, 1 productWithRatio (1, 2))
    assertEquals(2, 1 productWithRatio (2, 1))
    assertEquals(5, 10 productWithRatio (1, 2))
    assertEquals(20, 10 productWithRatio (2, 1))
  }
}

