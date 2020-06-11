package eddie

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, GridLayout}

import eddie.MandelImage.ColorInt
import javax.swing._

case class ColorColumn(hexField: JTextField, numField: JTextField)

case class Gradient(numGrad: Int, last: String, first: String = "000000", includeFirst: Boolean = false, includeLast: Boolean = true) {
  val firstColor = Color.decode(s"#$first")
  val lastColor = Color.decode(s"#$last")

  val gradations: List[ColorInt] = {
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
    if (includeLast) dropFirst else dropFirst.dropRight(1)
  }

  def toColumn: ColorPicker.Column = {
    ColorPicker.Column(first, last, gradations, includeFirst, includeLast)
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

  case class Column(topHexCode: String, botHexCode: String, colorList: List[ColorInt], includeTop: Boolean, includeBot: Boolean) {
    val colorSquares = createColorSquares(colorList)
    val topHexField = new JTextField(topHexCode)
    val botHexField = new JTextField(botHexCode)
    val numField = new JTextField(colorList.size.toString)
    val includeTopCheckbox = new JCheckBox()
    includeTopCheckbox.setSelected(includeTop)
    val includeBotCheckbox = new JCheckBox()
    includeBotCheckbox.setSelected(includeBot)
    val flipButton = new JButton("flip")
    flipButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = {
        val curTop = topHexField.getText
        topHexField.setText(botHexField.getText)
        botHexField.setText(curTop)
        val curTopCheck = includeTopCheckbox.getModel.isSelected
        includeTopCheckbox.setSelected(includeBotCheckbox.getModel.isSelected)
        includeBotCheckbox.setSelected(curTopCheck)
      }
    })
    topHexField.setMaximumSize(new Dimension(110, 30))
    botHexField.setMaximumSize(new Dimension(110, 30))
    numField.setMaximumSize(new Dimension(110, 30))
    flipButton.setMaximumSize(new Dimension(50, 30))

    val panel: JPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.setMaximumSize(new Dimension(70, 1000))
      p.add(numField)
      p.add(includeTopCheckbox)
      p.add(topHexField)
      p.add(colorSquares)
      p.add(botHexField)
      p.add(includeBotCheckbox)
      p.add(flipButton)
      p
    }

    def toGradient: Gradient = {
      val numGrad = Integer.parseInt(numField.getText)
      val includeTop = includeTopCheckbox.getModel.isSelected
      val includeBot = includeBotCheckbox.getModel.isSelected
      Gradient(numGrad, botHexField.getText, topHexField.getText, includeTop, includeBot)
    }
  }

  case class ColorPanel(gradients: List[Gradient]) {
    val allColorSquares = createColorSquares(gradients.flatMap(_.gradations), squareSize / 2)
    allColorSquares.setMaximumSize(new Dimension(100, 1000))
    val colorOptionsBox = new JComboBox(("none" :: MyColors.colors.map(_._1)).toArray)
    colorOptionsBox.setSelectedItem(MyColors.reverseMap.getOrElse(gradients, "none"))
    colorOptionsBox.setMaximumSize(new Dimension(100, 30))

    val presetPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS))
      val label = new JLabel("color presets:")
      label.setMaximumSize(new Dimension(100, 30))
      p.add(label)
      p.add(colorOptionsBox)
    }

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

    val helpButton = new JButton("Help")

    val panel: JPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.add(presetPanel)
      p.add(columnPanel)
      p.add(changeColorsButton)
      p.add(addRemovePanel)
      p.add(helpButton)
      p
    }
  }
}

object ColorPanelHelpText {
  val frame = new JFrame()
  val list = List(
    new JLabel("Column fields, top to bottom:"),
    new JLabel("Top text box is number of colors in the gradient"),
    new JLabel("Top check box is whether to include the top color itself in the gradient"),
    new JLabel("Next text box is the color of the top/beginning of the gradient"),
    new JLabel("text box below the gradient is bottom/end of the gradient"),
    new JLabel("Check box below that is whether to include the last color itself"),
    new JLabel("\"flip\" button will reverse the gradient"),
    new JLabel(""),
    new JLabel("You must click \"apply colors\" to change the image."),
    new JLabel(""),
    new JLabel("Excluding the beginning/end of a gradient is particularly useful if you want to start/end"),
    new JLabel("your gradient with black (000000),, but you want to fade towards black without actually"),
    new JLabel("ending with black itself."),
    new JLabel(""),
    new JLabel("You can add a gradient by using \"Add col at index\", specifying the index in the box to the right"),
    new JLabel("Index 0 will always add a gradient to the left of the first gradient"),
    new JLabel("Index 1 will add a gradient to the right of the first gradient"),
    new JLabel("If you have 4 gradients currently, Index 4 will add one to the right of all the gradients.")
  )
  frame.setLayout(new GridLayout(list.size, 1))
  frame.setSize(600, 400)
  list.foreach(frame.add)
}
