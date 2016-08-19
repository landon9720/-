package kuhn

import com.googlecode.lanterna.TextColor

object Colors {

  implicit class TextColorImplicits(c: TextColor) {

    def scale(factor: Double): TextColor = {
      new TextColor.RGB(
        (c.toColor.getRed*factor).toInt,
        (c.toColor.getGreen*factor).toInt,
        (c.toColor.getBlue*factor).toInt
      )
    }
    def darker: TextColor = {
      val darker = c.toColor.darker
      new TextColor.RGB(darker.getRed, darker.getGreen, darker.getBlue)
    }

    def brighter: TextColor = {
      val darker = c.toColor.brighter
      new TextColor.RGB(darker.getRed, darker.getGreen, darker.getBlue)
    }
  }

  val FG_VALUE = new TextColor.RGB(248, 236, 201)
  val FG = FG_VALUE.darker
  val FG_AXIS = FG.darker
  val BG = new TextColor.RGB(107, 83, 68).darker.darker
  val GRID_Y_STRIPE_BG = BG scale 0.8
  val TRANSPORT_CURSOR_BG = new TextColor.RGB(120, 30, 20)

}
