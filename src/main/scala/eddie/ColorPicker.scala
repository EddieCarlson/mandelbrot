package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.{Color, Dimension, FlowLayout, GridLayout}
import java.awt.image.BufferedImage

import eddie.MyImage.{ColorInt, MandelImage}
import javafx.scene.control.ColorPicker
import javax.swing.{BoxLayout, Icon, ImageIcon, JButton, JCheckBox, JFrame, JLabel, JPanel, JTextField}
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

case class ColorColumn(hexField: JTextField, numField: JTextField)

object ColorPicker {
  val squareSize = 50
  val maxPixels = 600

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

  case class Column(hexCode: String, colorList: List[ColorInt]) {
    val colorSquares = createColorSquares(colorList)
    val hexField = new JTextField(hexCode)
    val numField = new JTextField(colorList.size.toString)
    hexField.setMaximumSize(new Dimension(110, 30))
    numField.setMaximumSize(new Dimension(110, 30))
    val panel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.add(hexField)
      p.add(colorSquares)
      p.add(numField)
      p
    }
  }

  case class ColorPanel(colorInput: List[(String, Int)]) {
    val colorsWithHex = colorInput.map { case (hex, gradations) => (hex, expandBaseColor(hex, gradations)) }
    val colors = colorsWithHex.map(_._2)
    val allColorSquares = createColorSquares(colors.flatten, squareSize / 2)

    val columns = colorsWithHex.map(Function.tupled(Column.apply))
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

  val size = 64
  val sizeMinus1 = size - 1
  val mult = 256 / size

  val first = List.tabulate(size)(i => (255, i * mult, 0))
  val second = List.tabulate(size)(i => (255 - (i * mult), 255, 0))
  val third = List.tabulate(size)(i => (0, 255, i * mult))
  val fourth = List.tabulate(size)(i => (0, 255 - (i * mult), 255))
  val fifth = List.tabulate(size)(i => (i * mult, 0, 255))
  val sixth = List.tabulate(size)(i => (255, 0, 255 - (i * mult)))

  val colors = first ::: second ::: third ::: fourth ::: fifth ::: sixth

  val greys = 0.to(256).reverse
  val mults = List.tabulate(256)(i => i.toDouble / 256).reverse
  val multAndComplement = mults.zip(mults.reverse)

  val rectFrame = new JFrame
  rectFrame.setLayout(new FlowLayout)
  rectFrame.setSize(new Dimension(500, 300))
  val rectPanel = new JPanel
  rectPanel.setLayout(new BoxLayout(rectPanel, BoxLayout.X_AXIS))
  val rectLabel = new JLabel()
  val selectedColorLabel = new JLabel
  val copyButton = new JButton("copy hex")

  def setSelectedColor(colorInt: ColorInt): Unit = {
    val selectedImg = ColorPicker.colorRectImg(100, 100, colorInt)
    selectedColorLabel.setIcon(new ImageIcon(selectedImg))
    copyButton.getActionListeners.foreach(copyButton.removeActionListener)
    copyButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        val hexString = colorInt.toHexString.substring(2)
        println(hexString)
        Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(hexString), null)
      }
    })
    rectFrame.setVisible(true)
  }

  def makeGridFromColor(r: Int, g: Int, b: Int): BufferedImage = {
    val shades = mults.map { m => ((r * m).toInt, (g * m).toInt, (b * m).toInt) }
    val rectImg = new BufferedImage(256, 256, BufferedImage.TYPE_INT_ARGB)
    val colorInts = greys.zip(shades).zipWithIndex.map { case ((grey, (sr, sg, sb)), y) =>
      multAndComplement.zipWithIndex.map { case ((m1, m2), x) =>
        val multGrey = grey * m1
        val rr = (multGrey + sr * m2).toInt
        val gg = (multGrey + sg * m2).toInt
        val bb = (multGrey + sb * m2).toInt
        val colorInt = new Color(rr, gg, bb).getRGB
        rectImg.setRGB(x, y, colorInt)
        colorInt
      }
    }

    rectLabel.addMouseListener(new MouseListener {
      def mouseClicked(e: MouseEvent): Unit = {
        val (x, y) = (e.getX, e.getY)
        val colorInt = colorInts(y)(x)
        setSelectedColor(colorInt)
      }

      def mousePressed(e: MouseEvent): Unit = {}
      def mouseReleased(e: MouseEvent): Unit = {}
      def mouseEntered(e: MouseEvent): Unit = {}
      def mouseExited(e: MouseEvent): Unit = {}
    })
    rectImg
  }


  rectPanel.add(rectLabel)
  rectPanel.add(selectedColorLabel)
  rectPanel.add(copyButton)
  rectFrame.add(rectPanel)


  val frame = new JFrame
  frame.setLayout(new FlowLayout)
  frame.setSize(new Dimension(size + 100, size * 6 + 100))
  val panel = new JPanel
  panel.setLayout(new FlowLayout)
  val xSize = 30
  val img = new BufferedImage(xSize, size * 6, BufferedImage.TYPE_INT_ARGB)

  colors.zipWithIndex.foreach { case ((r, g, b), y) =>
    val colorInt = new Color(r, g, b).getRGB
    0.until(xSize).foreach { x =>
      img.setRGB(x, y, colorInt)
    }
  }

  val label = new JLabel()
  label.setIcon(new ImageIcon(img))
  label.addMouseListener(new MouseListener {
    def mouseClicked(e: MouseEvent): Unit = {
      val index = e.getY
      val (r, g, b) = colors(index)
      val gridImg = makeGridFromColor(r, g, b)
      rectLabel.setIcon(new ImageIcon(gridImg))
      setSelectedColor(new Color(r, g, b).getRGB)
    }

    def mousePressed(e: MouseEvent): Unit = {}
    def mouseReleased(e: MouseEvent): Unit = {}
    def mouseEntered(e: MouseEvent): Unit = {}
    def mouseExited(e: MouseEvent): Unit = {}
  })
  panel.add(label)
  frame.add(panel)
  frame.setVisible(true)
}

object Z extends App {
  val input = List(("ff007f", 4), ("9933ff", 4), ("00BFBF", 4))
  val frame = new JFrame()
}
