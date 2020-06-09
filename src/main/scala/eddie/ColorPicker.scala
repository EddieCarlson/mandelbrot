package eddie

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}

import eddie.MyImage.ColorInt
import javax.swing._

case class ColorColumn(hexField: JTextField, numField: JTextField)

case class Gradient(numGrad: Int, first: String, last: String = "000000", includeFirst: Boolean = true, includeLast: Boolean = false) {
  val firstColor = Color.decode(s"#$first")
  val lastColor = Color.decode(s"#$last")

  val gradations = {
    val extraFirst = if (includeFirst) 0 else 1
    val extraLast = if (includeLast) 0 else 1
    val numGradations = numGrad + extraFirst + extraLast
    val mults = 0.until(numGradations).map { i => i.toDouble / (numGradations - 1) }
    val multsAndComplement = mults.reverse.zip(mults)
    val gradations = multsAndComplement.map { case (m, m2) =>
      ColorPicker.wAvg(firstColor.getRed, firstColor.getGreen, firstColor.getBlue, m,
        lastColor.getRed, lastColor.getGreen, lastColor.getBlue, m2)
    }.toList
    val dropFirst = if (includeFirst) gradations else gradations.drop(1)
    val dropLast = if (includeLast) dropFirst else dropFirst.dropRight(1)
    dropLast.reverse
  }

  def toColumn = {
    ColorPicker.Column(first, last, gradations)
  }
}

object Gradient {
  def fromColor(c: Color, n: Int) = {
    Gradient(n, c.getRGB.toHexString.substring(2))
  }
}

object ColorPicker {
  val squareSize = 50
  val maxPixels = 600

  def wAvg(r: Int, g: Int, b: Int, m: Double, r2: Int, g2: Int, b2: Int, m2: Double) = {
    val rr = (r * m + r2 * m2).toInt
    val gg = (g * m + g2 * m2).toInt
    val bb = (b * m + b2 * m2).toInt
    new Color(rr, gg, bb).getRGB
  }


  def colorRectImg(x: Int, y: Int, color: ColorInt): BufferedImage = {
    val img = new BufferedImage(x, y, BufferedImage.TYPE_INT_ARGB)
    0.until(x).foreach { xIndex =>
      0.until(y).foreach { yIndex =>
        img.setRGB(xIndex, yIndex, color)
      }
    }
    img
  }

  def colorRectLabel(x: Int, y: Int, color: ColorInt): JLabel = {
    val img = colorRectImg(x, y, color)
    val label = new JLabel()
    label.setIcon(new ImageIcon(img))
    label
  }


  def mult(c: Color, f: Double): Color = {
    new Color((c.getRed * f).toInt, (c.getGreen * f).toInt, (c.getBlue * f).toInt)
  }

  def expandBaseColor(hex: String, gradations: Int): List[ColorInt] = {
    val color = Color.decode(s"#$hex")
    val multipliers = 0.until(gradations).map { i => 1 - (i.toDouble / gradations) }.reverse.toList
    multipliers.map(mult(color, _)).map(_.getRGB)
  }

  def expandBaseColors(baseColors: List[(Color, Int)]): List[List[ColorInt]] = {
    baseColors.map { case (color, num) =>
      val multipliers = 0.until(num).map { i => 1 - (i.toDouble / num) }.reverse.toList
      multipliers.map(mult(color, _)).map(_.getRGB)
    }
  }

  def createColorSquares(colors: List[ColorInt], height: Int = squareSize): JPanel = {
    val realHeight = Math.max(Math.min(height, maxPixels / (colors.size + 1)), 1)
    val panel = new JPanel()
    val colorSquares = colors.map { c => colorRectLabel(squareSize, realHeight, c) }
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))
    colorSquares.foreach(panel.add)
    panel
  }

  case class Column(topHexCode: String, botHexCode: String, colorList: List[ColorInt]) {
    val colorSquares = createColorSquares(colorList)
    val topHexField = new JTextField(topHexCode)
    val botHexField = new JTextField(botHexCode)
    val numField = new JTextField(colorList.size.toString)
    topHexField.setMaximumSize(new Dimension(110, 30))
    botHexField.setMaximumSize(new Dimension(110, 30))
    numField.setMaximumSize(new Dimension(110, 30))
    val panel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.add(numField)
      p.add(topHexField)
      p.add(colorSquares)
      p.add(botHexField)
      p
    }
  }

  case class ColorPanel(gradients: List[Gradient]) {
    val allColorSquares = createColorSquares(gradients.flatMap(_.gradations), squareSize / 2)

    val columns = gradients.map(_.toColumn)
    val columnPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS))
      columns.foreach(c => p.add(c.panel))
      p.add(allColorSquares)
      p
    }

    val changeColorsButton = new JButton("change colors")
    val addColumn = new JButton("add col at index:")
    val removeColumn = new JButton("remove col (idx):")
    val colIndex = new JTextField("0")
    colIndex.setMaximumSize(new Dimension(50, 30))

    val addRemovePanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS))
      val addRemoveP = new JPanel()
      addRemoveP.setLayout(new BoxLayout(addRemoveP, BoxLayout.Y_AXIS))
      addRemoveP.add(addColumn)
      addRemoveP.add(removeColumn)
      val indexP = new JPanel()
      indexP.setLayout(new BoxLayout(indexP, BoxLayout.Y_AXIS))
      indexP.add(colIndex)
      p.add(addRemoveP)
      p.add(indexP)
      p
    }

    val panel: JPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.add(columnPanel)
      p.add(changeColorsButton)
      p.add(addRemovePanel)
      p
    }
  }
}

object HexGrid extends App {
  def colorInt(i: Int) = {
    val str = i.toHexString
    val paddedStr = if (str.length > 6) {
      str.substring(str.length - 6)
    } else {
      val padding = 6 - str.length
      s"${"0" * padding}$str"
    }
    Color.decode(s"#$paddedStr").getRGB
  }

}

object Z extends App {
  val input = List(("ff007f", 4), ("9933ff", 4), ("00BFBF", 4))
  val frame = new JFrame()
}
