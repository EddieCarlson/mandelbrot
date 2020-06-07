package eddie

case class Grid(rPixels: Int, rMin: Double = -2, rMax: Double = 1, iMin: Double = -1, iMax: Double = 1) {
  val rSize = rMax - rMin
  val iSize = iMax - iMin
  val pixelsPerUnit = rPixels / rSize
  val iPixels = (pixelsPerUnit * iSize).toInt
  val totalPixels = rPixels * iPixels

  def iAtIndex(i: Int) = iMax - (iSize * i / iPixels)
  def rAtIndex(i: Int) = rMin + (rSize * i / rPixels)

  val rows = Stream.tabulate(iPixels)(iAtIndex)
  val row = Stream.tabulate(rPixels)(rAtIndex)

  def gridPixels: Stream[Stream[Complex]] = rows.map { yVal => row.map(Complex(_, yVal)) }
//  val gridPixels: Stream[Stream[Complex]] = rows.map { yVal => row.map(Complex(_, yVal)) }

  def pixelToCoord(x: Int, y: Int): (Double, Double) = (rAtIndex(x), iAtIndex(y))

  def zoomCenteredOn(x: Int, y: Int): Grid = {
    val (r, i) = pixelToCoord(x, y)
    val quarterR = rSize / 4
    val quarterI = iSize / 4
    Grid(rPixels, r - quarterR, r + quarterR, i - quarterI, i + quarterI)
  }

  def zoomOut: Grid = {
    val rCenter = (rMax + rMin) / 2
    val iCenter = (iMax + iMin) / 2
    Grid(rPixels, rCenter - rSize, rCenter + rSize, iCenter - iSize, iCenter + iSize)
  }
}