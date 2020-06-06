package eddie

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, FlowLayout, GridLayout}
import java.awt.image.BufferedImage

import eddie.MyImage.{ColorInt, MandelImage}
import javafx.scene.control.ColorPicker
import javax.swing.{BoxLayout, Icon, ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}

case class ColorColumn(hexField: JTextField, numField: JTextField)

object ColorPicker {
  val squareSize = 50

  def colorRect(x: Int, y: Int, color: ColorInt): JLabel = {
    val img = new BufferedImage(x, y, BufferedImage.TYPE_INT_ARGB)
    0.until(x).foreach { xIndex =>
      0.until(y).foreach { yIndex =>
        img.setRGB(xIndex, yIndex, color)
      }
    }
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
    val panel = new JPanel()
    val colorSquares = colors.map { c => colorRect(squareSize, height, c) }
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))
    colorSquares.foreach(panel.add)
    panel
  }

  case class Column(hexCode: String, colorList: List[ColorInt]) {
    val colorSquares = createColorSquares(colorList)
    val hexField = new JTextField(hexCode)
    val numField = new JTextField(colorList.size.toString)
    val panel = {
      val p = new JPanel()
      p.setLayout(new GridLayout(3, 1))
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
    val allColorSquares = createColorSquares(colors.flatten, squareSize / colors.size)

    val columns = colorsWithHex.map(Function.tupled(Column.apply))
    val columnPanel = {
      val p = new JPanel()
      p.setLayout(new GridLayout(1, columns.size + 1))
      columns.foreach(c => p.add(c.panel))
      p.add(allColorSquares)
      p
    }

    val changeColorsButton = new JButton("change colors")

    val panel: JPanel = {
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      p.add(columnPanel)
      p.add(changeColorsButton)
      p
    }
  }
}

object Z extends App {
  val input = List(("ff007f", 4), ("9933ff", 4), ("00BFBF", 4))
  val frame = new JFrame()
}
