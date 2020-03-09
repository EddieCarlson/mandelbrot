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

// TODO: colors are all fucked up - why does awt want the conjugate of the hex value?
object MyColors {
  val reds = List(
    (0x330019, 5),
    (0x99004c, 10),
    (0xcc0066, 15),
    (0xff007f, 20)
  )

  val purps = List(
    (0x330066, 25),
    (0x4c0099, 30),
    (0x6600cc, 35),
    (0x7f00ff, 40),
    (0x9933ff, 45)
  )

  val blues = List(
    (0x006666, 60),
    (0x009999, 80),
    (0x00CCCC, 120),
    (0x00FFFF, 250),
  )

  val lightCyan = ~0x99FFFF

  val colors = (reds ::: purps ::: blues).reverse.map { case (color, bound) => (~color, bound) }

  def chooseColor(count: Int) = {
    colors.find { case (_, bound) => count > bound }.map(_._1)
  }
}

object Mandelbrot extends App {
  def f(z: Complex, c: Complex): Complex = z.squared + c

  // returns the
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

  println("creating grid")
  val g = new Grid(5000)

  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)

  def toColor(c: Complex): Int = {
    val count = boundedCount(c)
    count.flatMap(MyColors.chooseColor).getOrElse(MyColors.lightCyan)
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
  val outputFile = new File(s"$personalLaptopDir/testAwtMandelbrot4.png")
  ImageIO.write(img, "png", outputFile)
}
