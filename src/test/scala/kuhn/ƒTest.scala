package kuhn

import org.junit.Test
import Monad._
import ƒ._
import Notes._

class ƒTest extends Song {
  override def song: Monad = ???

  @Test
  def test1: Unit = {
    println(Part(Sequence((0, 0, 1), (1, 0, 1))).normalize(24))
  }
  @Test
  def test2: Unit = {
    println(Part(Sequence((0, 0, 12),(12, 0, 12))).normalize(beats))
  }
  @Test
  def test3: Unit = {
    val x = Start > {
      implicit val scale = CMajor
      n(-N7, -N6, -N5, -N4, -N3, -N2, -N1, N0, N1, N2, N3, N4, N5, N6, N7)
    } > End
    println(x)
  }
}
