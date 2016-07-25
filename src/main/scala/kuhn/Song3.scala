package kuhn

import kuhn.Notes._

object Song3 extends Song {
  val part1 = b(C4) > b(C4, E4) > b(C4, E4, G4, C5)
  val song =
    Start >
      part1 >
      (part1 up 2) >
      (part1 up 4) >
      (part1 up 2) ===
      ((part1 down 12) ** 4)
    End
}
