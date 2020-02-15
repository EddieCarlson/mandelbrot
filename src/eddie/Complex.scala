package eddie

import doodle.image._
import doodle.core._
import doodle.image.syntax._
import doodle.java2d._
import doodle.effect.Writer._
import doodle.image.Image.Elements.Empty

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

class Grid(rPixels: Int, rMin: Double = -2, rMax: Double = 1, iMin: Double = -1, iMax: Double = 1) {
  val rSize = rMax - rMin
  val iSize = iMax - iMin
  val pixelsPerUnit = rPixels / rSize
  val iPixels = pixelsPerUnit * iSize
  println(pixelsPerUnit)
  println(rSize)

  def pixels(size: Double, offset: Double): Seq[Double] =
    1.to((size * pixelsPerUnit).toInt).map(d => (d.toDouble / pixelsPerUnit) + offset)

  val column = pixels(iSize, iMin)
  val row = pixels(rSize, rMin)

  val grid = column.map { yVal => row.map(Complex(_, yVal)) }.reverse
  println(grid.size)
  println(grid.head.size)


  def toImage2(f: Complex => Boolean): Image = {
    grid.map { rows =>
      rows.map { c =>
        val color = if (f(c)) Color.purple else Color.lightBlue
        Image.rectangle(5, 5).fillColor(color)
      }.foldLeft(Image.empty)(_.beside(_))
    }.foldLeft(Image.empty)(_.above(_))
  }
//
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

  def display(f: Complex => Boolean) =
    grid.map(_.map(c => if (f(c)) "x" else " ")).foreach(d => println(d.mkString))
}

object Mandelbrot extends App {
  def f(z: Complex, c: Complex): Complex = z.squared + c

  def bounded(c: Complex, z: Complex = Complex.zero, count: Int = 0): Boolean =
    count >= 1000 || (!z.outOfBounds && bounded(c, f(z, c), count + 1))

  val blackSquare = Image.rectangle(30, 30).fillColor(Color.lightBlue)
  val redSquare = Image.rectangle(30, 30).fillColor(Color.purple)

  // A chessboard, broken into steps showing the recursive construction
  val twoByTwo =
    (redSquare.beside(blackSquare))
      .above(blackSquare.beside(redSquare))

  val fourByFour =
    (twoByTwo.beside(twoByTwo))
      .above(twoByTwo.beside(twoByTwo))

  val chessboard =
    (fourByFour.beside(fourByFour))
      .above(fourByFour.beside(fourByFour))

  chessboard.write[Png]("/Users/eddie.carlson/developer/eddie/mandelbrot/chess.png")

  val i = new Grid(600).toImage2(bounded(_))
  println("out")
  i.write[Png]("/Users/eddie.carlson/developer/eddie/mandelbrot/test6.png")
//  new Grid(500, -.8, -.7, .1, .2).display(bounded(_))
//  new Grid(500, -.75, -.725, .125, .175).display(bounded(_))
}
