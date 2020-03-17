package eddie

case class Grid(rPixels: Int, rMin: Double = -2, rMax: Double = 1, iMin: Double = -1, iMax: Double = 1) {
  val rSize = rMax - rMin
  val iSize = iMax - iMin
  val pixelsPerUnit = rPixels / rSize
  val iPixels = (pixelsPerUnit * iSize).toInt
  val totalPixels = rPixels * iPixels

  val rows = Stream.tabulate(iPixels)(n => iMax - (iSize * n / iPixels))
  val row = Stream.tabulate(rPixels)(n => rMin + (rSize * n / rPixels))

  def gridPixels: Stream[Stream[Complex]] = rows.map { yVal => row.map(Complex(_, yVal)) }
}