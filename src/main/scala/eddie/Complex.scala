package eddie

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO


case class Complex(real: Double, imaginary: Double) {
  def outOfBounds: Boolean = Math.abs(real) > 2 || Math.abs(imaginary) > 2

  def squared: Complex = {
    val realReal = real * real
    val realImag = real * imaginary
    val imagImag = imaginary * imaginary
    Complex(realReal - imagImag, realImag * 2)
  }

  def +(other: Complex): Complex = Complex(real + other.real, imaginary + other.imaginary)
}

object Complex {
  val zero = Complex(0, 0)
}

// TODO: set is symmetrical across x axis, only calculate half
class Grid(val rPixels: Int, rMin: Double = -2, rMax: Double = 1, iMin: Double = -1, iMax: Double = 1) {
  val rSize = rMax - rMin
  val iSize = iMax - iMin
  val pixelsPerUnit = rPixels / rSize
  val iPixels = (pixelsPerUnit * iSize).toInt
  val totalPixels = rPixels * iPixels

  val column = pixels(iSize, iMin)
  val row = pixels(rSize, rMin)

  val gridPixels = column.map { yVal => row.map(Complex(_, yVal)) }.reverse

  private def pixels(size: Double, offset: Double): Seq[Double] =
    1.to((size * pixelsPerUnit).toInt).map(d => (d.toDouble / pixelsPerUnit) + offset)
}

object Mandelbrot extends App {
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

  println("creating grid")
  val g = new Grid(5000)

  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)

  def toColor(c: Complex): Int = {
    val count = boundedCount(c)
    count.map(MyColors.chooseColor).getOrElse(MyColors.lightCyan)
  }

  println("converting to colors")
  val colorGrid = g.gridPixels.par.map(_.map(toColor))

  println("assigning colors")
  colorGrid.zipWithIndex.foreach { case (row, h) =>
    row.zipWithIndex.foreach { case (color, w) =>
      img.setRGB(w, h, color)
    }
  }

  println("writing file")
  val outputFile = new File(s"$workLaptopDir/sj1.png")
  ImageIO.write(img, "png", outputFile)
}
