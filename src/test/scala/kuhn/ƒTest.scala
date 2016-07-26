package kuhn

import org.junit.Test
import org.junit.Assert._
import Song._

class ƒTest {
  @Test
  def test1 = {
    assertEquals("""Part(events
                   |Note(0,0,12,64,64)
                   |duration=12)""".stripMargin, n(0).toString)
    assertEquals("""Part(events
                   |Note(0,0,12,64,64)
                   |Note(12,1,12,64,64)
                   |duration=24)""".stripMargin, n(0, 1).toString)
    assertEquals("""""".stripMargin, (n(0) N).toString)
    assertEquals("""""".stripMargin, (n(0, 1) N).toString)
  }
//  @Test
//  def test2: Unit = {
//    println(Part(Sequence((0, 0, 12),(12, 0, 12))).normalize(beats))
//  }
//  @Test
//  def test3: Unit = {
//    val x = Start > {
//      implicit val scale = CMajor
//      n(-N7, -N6, -N5, -N4, -N3, -N2, -N1, N0, N1, N2, N3, N4, N5, N6, N7)
//    } > End
//    println(x)
//  }
}
