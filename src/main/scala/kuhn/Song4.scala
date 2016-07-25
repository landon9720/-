package kuhn

import ƒ._
import kuhn.Notes._

object Song4 extends Song {

  def p(implicit s: Scale) =
    b(N0) > b(N0, N1) > b(N3) > b(N0) > b(N0, N1) > b(N4) > b(N0) > b(N0, N1) > b(N6) > b(N6, N6) > b(N6) > b(N0)

  val song =
    Start > {
      implicit val scale = DMajor
      n(-N7, -N6, -N5, -N4, -N3, -N2, -N1, N0, N1, N2, N3, N4, N5, N6, N7)
    } > {
      implicit val scale = DMinor
      n(-N7, -N6, -N5, -N4, -N3, -N2, -N1, N0, N1, N2, N3, N4, N5, N6, N7)
    } > {
      implicit val scale: Scale = DMajor
      p > (p up 2) > (p up 4) > (p up 5)
    } > {
      implicit val scale: Scale = DMinor
      p > (p up 2) > (p up 4) > (p up 5)
    } > End
}
