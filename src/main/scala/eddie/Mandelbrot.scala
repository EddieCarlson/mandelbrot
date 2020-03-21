package eddie

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object Mandelbrot extends App {
  val start = System.currentTimeMillis

  def f(z: Complex, c: Complex): Complex = z.squared + c

  // returns the number of iterations of `f` applied to `c` to yield a "large" value. returns None if still bounded
  // after 1000 iterations
  def boundedCount(c: Complex, z: Complex = Complex.zero, count: Int = 0): Option[Int] = {
    if (z.outOfBounds) {
      Some(count)
    } else if (count > 1000) {
      None
    } else {
      boundedCount(c, f(z, c), count + 1)
    }
  }

  val personalLaptopDir = "/Users/eddie/IdeaProjects/mandelbrot"
  val workLaptopDir = "/Users/eddie.carlson/developer/eddie/mandelbrot"

  //  val g = new HalfGrid(20000)
  //  val g = new Grid(900)

//  val g = new Grid(rPixels = 120, rMin = 1.16 - 2, rMax = 1.30 - 2, iMin = 1.0356 - 1, iMax = 1.259 - 1)
//    val g = new Grid(rPixels = 15000, rMin = -0.6704, rMax = -0.41495, iMin = 0.5063, iMax = 0.7196)
    val g = Grid(rPixels = 24000, rMin = -2, rMax = 1, iMin = 0, iMax = 1)

  val mirrorGate = true
  val mirror = mirrorGate && g.iMin == 0

  val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

  //  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)
  val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

  def toColor(c: Complex): Int = {
    val count = boundedCount(c)
    count.map(MyColors.chooseColor).getOrElse(MyColors.blue.getRGB)
  }

  def cGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
    (row.map(toColor).zipWithIndex, h)
  }

  val setRGB = if (mirror) {
    (w: Int, h: Int, c: Int) => {
      img.setRGB(w, h, c)
      img.setRGB(w, h + ((g.iPixels - h) * 2) - 1, c)
    }
  } else {
    (w: Int, h: Int, c: Int) => img.setRGB(w, h, c)
  }

  cGrid.foreach { case (row, h) =>
    if (h % 1000 == 0) println(h)
    row.foreach { case (color, w) =>
      setRGB(w, h, color)
    }
  }

  println("writing file")
  val outputFile = new File(s"$personalLaptopDir/manycolor1.png")
  ImageIO.write(img, "png", outputFile)

  println()
  println(s"took: ${(System.currentTimeMillis - start) / 1000} seconds")
}
