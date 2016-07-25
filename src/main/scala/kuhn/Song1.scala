package kuhn

import ƒ._
import Notes._

object Song1 extends Song {
  def part(a: Int, b: Int) = notes(
    (0, a+0, beats),
    (beats/2, a+b, beats-beats/2)
  ) ** (2*beats)
  val song = {
    for {
      a ← 0 to 12
      b ← 0 to 12
    } yield part(a, b-a)
  } reduce (_>_)
}
