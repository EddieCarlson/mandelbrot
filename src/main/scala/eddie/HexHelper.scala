package eddie

import java.awt.datatransfer.StringSelection
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, FlowLayout, Toolkit}

import eddie.MyImage.ColorInt
import javax.swing._

object HexHelper {
  val size = 64
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

  val rectPanel = new JPanel
  rectPanel.setLayout(new BoxLayout(rectPanel, BoxLayout.X_AXIS))
  val rectLabel = new JLabel()
  val selectedColorLabel = new JLabel
  val copyButton = new JButton("copy hex")

  rectPanel.add(rectLabel)
  rectPanel
  rectPanel.add(Box.createRigidArea(new Dimension(5, 0)))
  val selectedPanel = new JPanel()
  selectedPanel.setLayout(new BoxLayout(selectedPanel, BoxLayout.Y_AXIS))
  selectedPanel.add(selectedColorLabel)
  selectedPanel.add(Box.createRigidArea(new Dimension(0, 20)))
  selectedPanel.add(copyButton)
  rectPanel.add(selectedPanel)

  val frame = new JFrame
  frame.setLayout(new FlowLayout)
  frame.setSize(new Dimension(350 + size, size * 6 + 30))
  val panel = new JPanel
  panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS))
  val xSize = 30
  val img = new BufferedImage(xSize, size * 6, BufferedImage.TYPE_INT_ARGB)

  def setSelectedColor(colorInt: ColorInt): Unit = {
    val selectedImg = ColorPicker.colorRectImg(100, 100, colorInt)
    selectedColorLabel.setIcon(new ImageIcon(selectedImg))
    copyButton.getActionListeners.foreach(copyButton.removeActionListener)
    copyButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        val hexString = colorInt.toHexString.substring(2)
        Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(hexString), null)
      }
    })
    frame.setVisible(true)
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

  colors.zipWithIndex.foreach { case ((r, g, b), y) =>
    val colorInt = new Color(r, g, b).getRGB
    0.until(xSize).foreach { x =>
      img.setRGB(x, y, colorInt)
    }
  }

  def setRectColor(r: Int, g: Int, b: Int) = {
    val gridImg = makeGridFromColor(r, g, b)
    rectLabel.setIcon(new ImageIcon(gridImg))
    setSelectedColor(new Color(r, g, b).getRGB)
  }

  val label = new JLabel()
  label.setIcon(new ImageIcon(img))
  label.addMouseListener(new MouseListener {
    def mouseClicked(e: MouseEvent): Unit = {
      val index = e.getY
      val (r, g, b) = colors(index)
      setRectColor(r, g, b)
    }

    def mousePressed(e: MouseEvent): Unit = {}
    def mouseReleased(e: MouseEvent): Unit = {}
    def mouseEntered(e: MouseEvent): Unit = {}
    def mouseExited(e: MouseEvent): Unit = {}
  })
  panel.add(label)
  frame.add(panel)
  frame.add(rectPanel)

  setRectColor(0, 200, 255)
}
