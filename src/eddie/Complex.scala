package eddie


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

  def display(f: Complex => Boolean) =
    grid.map(_.map(c => if (f(c)) "x" else " ")).foreach(d => println(d.mkString))
}

object Mandelbrot extends App {
  def f(z: Complex, c: Complex): Complex = z.squared + c

  def bounded(c: Complex, z: Complex = Complex.zero, count: Int = 0): Boolean =
    count >= 1000 || (!z.outOfBounds && bounded(c, f(z, c), count + 1))

//  new Grid(100).display(bounded(_))
//  new Grid(500, -.8, -.7, .1, .2).display(bounded(_))
  new Grid(500, -.75, -.725, .125, .175).display(bounded(_))
}
