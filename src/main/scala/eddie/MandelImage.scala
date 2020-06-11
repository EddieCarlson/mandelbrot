package eddie

import java.awt.image.BufferedImage

import eddie.MandelImage.ColorInt

case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, List[(Int, Int)]], g: Grid, colors: List[Gradient] = MandelImage.initialBaseColors.map(Function.tupled(Gradient.fromColor))) {
  val autoColors: Map[Int, ColorInt] = {
    def group(remainingColors: List[Int], remainingGroups: List[(Int, Int)], curColor: Int, remainingPixelCount: Int, curPixels: Int = 0, acc: Map[Int, Int] = Map.empty): Map[Int, Int] = {
      if (remainingGroups.isEmpty) {
        acc
      } else if (remainingColors.isEmpty) {
        val remainingMap = remainingGroups.map(_._1).map(t => (t, curColor)).toMap
        acc ++ remainingMap
      } else if (curPixels.toDouble / remainingPixelCount < 1.0 / remainingColors.size.toDouble) {
        val (toAdd, size) :: tail = remainingGroups
        group(remainingColors, tail, curColor, remainingPixelCount - size, curPixels + size, acc + (toAdd -> curColor))
      } else {
        val nextColor :: tail = remainingColors
        group(tail, remainingGroups, nextColor, remainingPixelCount, 0, acc)
      }
    }
    val start = System.currentTimeMillis
    val nonMax = pixelGroups.mapValues(_.size).toList.sortBy(_._1).filterNot(_._1 == g.maxIterations)
    val setupD = System.currentTimeMillis - start
    println(s"autoColor setup: $setupD millis")
    val allColors = colors.flatMap(_.gradations)
    val notInColors = allColors.dropRight(1)
    val inColor = allColors.last
    val gro = group(notInColors.tail, nonMax, notInColors.head, nonMax.map(_._2).sum)
    //      println(gro.groupBy(_._2).mapValues(_.keys.flatMap(k => pixelGroups.getOrElse(k, Set.empty)).size).values) // this line is very expensive: 800 millis
    val duration = System.currentTimeMillis - start
    println(s"autoColors: $duration millis")
    gro + (g.maxIterations -> inColor)
  }

  val start = System.currentTimeMillis
  autoColors.foreach { case (bound, color) =>
    pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
      img.setRGB(x, y, color)
    }
  }
  val duration = System.currentTimeMillis - start
  println(s"applyColors: $duration millis")
}


object MandelImage{
  type ColorInt = Int

  val initialBaseColors = MyColors.candy
  val mirrorGate = true

  def fromGrid(g: Grid, colors: List[Gradient] = initialBaseColors.map(Function.tupled(Gradient.fromColor))): MandelImage = {
    println(g)
    val start = System.currentTimeMillis
    val mirror = mirrorGate && g.iMin == 0

    val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

    val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

    val pgStart = System.currentTimeMillis
    val pixelGroups = g.escapeBounds.groupBy(_._1).mapValues(_.map(_._2).toList)
    val pgDuration = System.currentTimeMillis - pgStart
    println(s"escape bounds and pixelGroups: $pgDuration millis")

    val mi = MandelImage(img, pixelGroups, g, colors)
    val duration = System.currentTimeMillis - start
    println(s"fromGrid: $duration millis")
    mi
  }
}

