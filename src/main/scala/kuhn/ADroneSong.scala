package kuhn

import kuhn.ƒ._
import Notes._

object ADroneSong extends Song {
  val song =
      Part(Sequence((0, A2, beats))) >
      Part(Sequence((0, A3, beats))) >
      Part(Sequence((0, A4, beats))) >
      Part(Sequence((0, A5, beats))) normalize beats * 64 * 4
}