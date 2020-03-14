package eddie

import java.awt.Color

object MyColors {
  val reds = List(
    ("#330019", 6),
    ("#99004c", 10),
    ("#cc0066", 15),
    ("#ff007f", 20)
  )

  val purps = List(
    ("#330066", 25),
    ("#4c0099", 30),
    ("#6600cc", 35),
    ("#7f00ff", 40),
    ("#9933ff", 45)
  )

  val blues = List(
    ("#006666", 60),
    ("#009999", 80),
    ("#00CCCC", 120),
    ("#00FFFF", 250),
  )

  val lightCyan = Color.decode("#99FFFF").getRGB
  val black = Color.decode("#000000").getRGB

  val colors = (reds ::: purps ::: blues).reverse.map { case (color, bound) => (Color.decode(color).getRGB, bound) }

  def chooseColor(count: Int): Int = {
    colors.find { case (_, bound) => count > bound }.map(_._1).getOrElse(black)
  }
}
