package eddie

import java.awt.Component
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.util.Random

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

  val grid = column.map { yVal => row.map(Complex(_, yVal)) }.zipWithIndex.map {
    case (row, yIndex) => row.zipWithIndex.map {
      case (c, rIndex) => (c, (rIndex, yIndex))
    }
  }.reverse

  val gridNums = grid.map(_.map(_._1))

//  grid.map(x => println(x.map { case (c, _) => toChar(c, Mandelbrot.bounded)}.mkString("")))
//  grid.map(x => println(x.map { case (_, ee) => ee.toString}.mkString("")))
//
//  def toChar(c: Complex, f: (Complex, Complex, Int) => Boolean) = {
//    if (f(c, Complex.zero, 0)) {
//      "x"
//    } else {
//      " "
//    }
//  }

  private def pixels(size: Double, offset: Double): Seq[Double] =
    1.to((size * pixelsPerUnit).toInt).map(d => (d.toDouble / pixelsPerUnit) + offset)

//  def toImage2(f: Complex => Boolean): Image = {
//    grid.map { rows =>
//      rows.map { c =>
//        val color = if (f(c)) Color.purple else Color.lightBlue
//        Image.rectangle(5, 5).fillColor(color)
//      }.foldLeft(Image.empty)(_.beside(_))
//    }.foldLeft(Image.empty)(_.above(_))
//  }

//  def escapeBoundIterCounts(f: Complex => Int): Map[Int, Double] = {
//    grid.flatMap { rows => rows.map(f) }.groupBy(identity).mapValues(_.size / totalPixels)
//  }

//  def toImage(f: Complex => Boolean): Image = {
//    grid.foldLeft(Empty: Image) { (i, row) =>
//      println("a")
//      i.above {
//        row.foldLeft(i) { (iRow, c) =>
//          println(".")
//          val color = if (f(c)) Color.black else Color.white
//          val image = Image.rectangle(30, 30).fillColor(color)
//          iRow.beside(image)
//        }
//      }
//    }
//  }

//  def display(f: Complex => Boolean) =
//    grid.map(_.map(c => if (f(c)) "x" else " ")).foreach(d => println(d.mkString))
}

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
    (0x99FFFF, 1000)
  )

  val colors = (reds ::: purps ::: blues).reverse.map { case (color, bound) => (~color, bound) }

  def chooseColor(count: Int) = {
    colors.find { case (_, bound) => count > bound }.getOrElse(colors.head)._1
  }
}

object Mandelbrot extends App {
  def f(z: Complex, c: Complex): Complex = z.squared + c

  def bounded(c: Complex, z: Complex = Complex.zero, count: Int = 0): Boolean =
    count >= 1000 || (!z.outOfBounds && bounded(c, f(z, c), count + 1))

  def boundedCount(c: Complex, z: Complex = Complex.zero, count: Int = 0): Int = {
    if (count >= 1000 || z.outOfBounds) {
      count
    } else {
      boundedCount(c, f(z, c), count + 1)
    }
  }

  val personalLaptopDir = "/Users/eddie/IdeaProjects/mandelbrot"

  println("creating grid")
  val g = new Grid(500)

  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)

  def toColor(c: Complex) = {
    val count = boundedCount(c)

    val cc = MyColors.chooseColor(count)
//    println(cc)
    cc

//    if (count == 1000) Color.CYAN
//    else if (count > 400) Color.RED
//    else if (count > 100) Color.ORANGE
//    else if (count > 70) Color.YELLOW
//    else if (count > 25) Color.GREEN
//    else if (count > 15) Color.LIGHT_GRAY
//    else if (count > 10) Color.GRAY
//    else if (count > 5) Color.DARK_GRAY
//    else Color.BLACK
  }

  println("converting to colors")
  val colorGrid = g.gridNums.par.map(_.map(toColor))

  println("assigning colors")
  colorGrid.zipWithIndex.foreach { case (row, h) =>
    row.zipWithIndex.foreach { case (color, w) =>
      img.setRGB(w, h, color)
    }
  }

//  val width = 60
//  val height = 40
//  val canvas = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
//
//  0.until(width).foreach { w => 0.until(height).foreach { h => canvas.setRGB(w, h, Color.WHITE.getRGB) } }
//
//  canvas.setRGB(3, 10, Color.BLACK.getRGB)

  println("writing file")
  val outputFile = new File(s"$personalLaptopDir/testAwtMandelbrot4.png")
  ImageIO.write(img, "png", outputFile)

//  val i = new Grid(600).toImage2(bounded(_))
//  println(new Grid(600).escapeBoundIterCounts(boundedCount(_)).toList.sortBy(_._2).reverse)
//  println("out")
//  i.write[Png]("/Users/eddie.carlson/developer/eddie/mandelbrot/test6.png")
//  new Grid(500, -.8, -.7, .1, .2).display(bounded(_))
//  new Grid(500, -.75, -.725, .125, .175).display(bounded(_))
}
