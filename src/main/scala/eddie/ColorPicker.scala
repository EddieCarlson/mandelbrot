package eddie

import java.awt.{Color, FlowLayout, GridLayout}
import java.awt.image.BufferedImage

import eddie.MyImage.ColorInt
import javafx.scene.control.ColorPicker
import javax.swing.{Icon, ImageIcon, JFrame, JLabel, JPanel}

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

  def createColumn(colors: List[ColorInt], height: Int = squareSize): JPanel = {
    val panel = new JPanel()
    val colorSquares = colors.map { c => colorRect(squareSize, height, c) }
    panel.setLayout(new GridLayout(colorSquares.size, 1))
    colorSquares.foreach(panel.add)
    panel
  }

  def create(colorCols: List[List[ColorInt]]): JPanel = {
    val panel = new JPanel()
    val columns = colorCols.map(createColumn(_))
    val allColorColumn = createColumn(colorCols.flatten, squareSize / 3)
    val allColumns = columns :+ allColorColumn
    panel.setLayout(new GridLayout(1, allColumns.size))
    allColumns.foreach(panel.add)

    panel
  }

}

object Z extends App {
  val colors = List(Color.decode("#00ff00"), Color.decode("#00cc00"), Color.decode("#009900")).map(_.getRGB)
  val colors2 = List(Color.decode("#0000ff"), Color.decode("#0000cc"), Color.decode("#000099")).map(_.getRGB)
  val colorCols = List(colors, colors2)
  val frame = new JFrame()
  frame.setLayout(new FlowLayout)
  frame.setSize(1000, 700)
  frame.setVisible(true)
  val panel = ColorPicker.create(colorCols)
  frame.add(panel)
  frame.setVisible(true)
  Thread.sleep(2000)
  val colorCols2 = List(colors2, colors)
  val panel2 = ColorPicker.create(colorCols2)
  frame.remove(panel)
  frame.add(panel2)
  frame.setVisible(true)
}
