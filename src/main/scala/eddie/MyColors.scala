package eddie

import java.awt.Color

object MyColors {
//  val reds = List(
//    ("#330019", 6),
//    ("#99004c", 10),
//    ("#cc0066", 15),
//    ("#ff007f", 20)
//  )

  def mult(c: Color, f: Double) = {
    new Color((c.getRed * f).toInt, (c.getGreen * f).toInt, (c.getBlue * f).toInt)
  }

  val black = Color.BLACK

  val grey = new Color(140, 140, 140)
  val greys = black :: 1.to(6).map(_ / 6.0).map(mult(grey, _)).toList

  val red = new Color(255, 0, 128)
  val reds = 4.to(10).map(_ / 10.0).map(mult(red, _)).toList

  val purp = new Color(170, 84, 255)
  val purps = 4.to(10).map(_ / 10.0).map(mult(purp, _)).toList

  val yellow = new Color(255, 252, 97)
  val yellows = 5.to(10).map(_ / 10.0).map(mult(yellow, _)).toList

  val blue = new Color(84, 210, 255)
  val blues = 4.to(9).map(_ / 10.0).map(mult(blue, _)).toList

  val colorsAndIncs = List(
    (greys, 1),
    (reds, 2),
    (purps, 2),
    (yellows, 4),
    (blues, 7)
  )

  val colorThresholds = colorsAndIncs.foldLeft((0, List.empty[(Color, Int)])) {
    case ((prevThreshold, acc), (colorList, inc)) =>
      val (newThreshold, colorSet) = colorList.foldLeft((prevThreshold, List.empty[(Color, Int)])) {
        case ((prevThresh, curAcc), color) =>
          val nextThresh = prevThresh + inc
          (nextThresh, (color, nextThresh) :: curAcc)
      }
      (newThreshold, colorSet ::: acc)
  }._2.reverse.map { case (c, t) => (c.getRGB, t) }

//  Color.decode()
//
//  val reds =

  //  val reds = List(
  //    ("#330019", 6),
  //    ("#99004c", 10),
  //    ("#cc0066", 15),
  //    ("#ff007f", 20)
  //  )
//  val purps = List(
//    ("#330066", 25),
//    ("#4c0099", 30),
//    ("#6600cc", 35),
//    ("#7f00ff", 40),
//    ("#9933ff", 45)
//  )

//  val blues = List(
//    ("#006666", 60),
//    ("#009999", 80),
//    ("#00CCCC", 120),
//    ("#00FFFF", 250),
//  )
//
//  val lightCyan = Color.decode("#99FFFF").getRGB
//  val black = Color.decode("#000000").getRGB

//  val colors = (reds ::: purps ::: blues).reverse.map { case (color, bound) => (Color.decode(color).getRGB, bound) }

  def chooseColor(count: Int): Int = {
    colorThresholds.find { case (_, bound) => count < bound }.getOrElse(colorThresholds.last)._1
  }
}
