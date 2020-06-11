package eddie

import java.awt.FlowLayout
import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger

import eddie.ColorPicker.ColorPanel
import javax.imageio.ImageIO
import javax.swing._

object MandelbrotFunctions {
  val defaultMaxIterations = 1000

  def f(z: Complex, c: Complex): Complex = z.squared + c

  // returns the number of iterations of `f` applied to `c` to yield a "large" value. returns None if still bounded
  // after 1000 iterations
  def boundedCount(c: Complex, maxIterations: Int, z: Complex = Complex.zero, count: Int = 0): Option[Int] = {
    if (z.outOfBounds) {
      Some(count)
    } else if (count > maxIterations) {
      None
    } else {
      boundedCount(c, maxIterations, f(z, c), count + 1)
    }
  }
}

object Mandelbrot extends App {
  val g = Grid(rPixels = 1000)
  val startingMandelImg = MandelImage.fromGrid(g)

  val imgFrame = new JFrame()
  val lbl = new JLabel
  val zoomOutButton = new JButton("zoom out")
  val saveButton = new JButton("save at res:")
  val saveAs = new JTextField("my_image")
  val saveOptionList = List(
    "shown" -> None,
    "2K" -> Some(2000),
    "4K" -> Some(4000),
    "6K" -> Some(6000),
    "8K" -> Some(8000)
  )
  val saveOptionMap = saveOptionList.toMap
  val saveOptions = new JComboBox(saveOptionList.map(_._1).toArray)
  val colorPanelWrapper = new JPanel()
  imgFrame.setLayout(new FlowLayout)
  imgFrame.setSize(g.rPixels + 10, g.iPixels + 65)
  val sizeField = new JTextField(g.rPixels.toString)
  val sizeButton = new JButton("change size")
  imgFrame.add(lbl)
  imgFrame.add(new JLabel("xPixels:"))
  imgFrame.add(sizeField)
  imgFrame.add(sizeButton)
  imgFrame.add(zoomOutButton)
  imgFrame.add(saveButton)
  imgFrame.add(saveOptions)
  imgFrame.add(new JLabel("max iter:"))
  val iterationField = new JTextField(g.maxIterations.toString)
  val iterationButton = new JButton("set iterations")
  imgFrame.add(iterationField)
  imgFrame.add(iterationButton)
  val colorFrame = new JFrame()
  colorFrame.setLayout(new FlowLayout)
  colorFrame.setSize(400, 700)
  colorFrame.add(colorPanelWrapper)

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss")
  val date = new Date()
  val saveNum = new AtomicInteger(1)
  var lastSavePath: Option[String] = None

  def setEverything(mImg: MandelImage)(colorPanel: ColorPanel = ColorPanel(mImg.colors)): Unit = {
    lbl.setIcon(new ImageIcon(mImg.img))
    colorPanelWrapper.removeAll()
    colorPanelWrapper.add(colorPanel.panel)

    colorPanel.helpButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        ColorPanelHelpText.frame.setVisible(true)
      }
    })

    lbl.getMouseListeners.foreach(lbl.removeMouseListener)
    lbl.addMouseListener(new MouseListener {
      override def mouseReleased(mouseEvent: MouseEvent): Unit = {
        val point = mouseEvent.getPoint
        val newGrid = mImg.g.zoomCenteredOn(point.x, point.y)
        val newMImg = MandelImage.fromGrid(newGrid, mImg.colors)
        setEverything(newMImg)()
      }

      override def mousePressed(mouseEvent: MouseEvent): Unit = {}
      override def mouseClicked(mouseEvent: MouseEvent): Unit = {}
      override def mouseEntered(mouseEvent: MouseEvent): Unit = {}
      override def mouseExited(mouseEvent: MouseEvent): Unit = {}
    })

    colorPanel.changeColorsButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val newGradients = colorPanel.columns.map(_.toGradient)
        val newMImg = mImg.copy(colors = newGradients)
        setEverything(newMImg)()
      }
    })

    sizeButton.getActionListeners.foreach(sizeButton.removeActionListener)
    sizeButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        val newGrid = mImg.g.copy(rPixels = Integer.parseInt(sizeField.getText))
        imgFrame.setSize(newGrid.rPixels + 10, newGrid.iPixels + 65)
        setEverything(MandelImage.fromGrid(newGrid, mImg.colors))()
      }
    })

    iterationButton.getActionListeners.foreach(iterationButton.removeActionListener)
    iterationButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val newGrid = mImg.g.copy(maxIterations = Integer.parseInt(iterationField.getText))
        setEverything(MandelImage.fromGrid(newGrid, mImg.colors))()
      }
    })

    zoomOutButton.getActionListeners.foreach(zoomOutButton.removeActionListener)
    zoomOutButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        setEverything(MandelImage.fromGrid(mImg.g.zoomOut, mImg.colors))()
      }
    })

    saveButton.getActionListeners.foreach(saveButton.removeActionListener)
    saveButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val saveFrame = new JFrame()
        val saveFileChooser = new JFileChooser()
        lastSavePath.foreach(p => saveFileChooser.setCurrentDirectory(new File(p)))
        val selection = saveFileChooser.showSaveDialog(saveFrame)
        if (selection == JFileChooser.APPROVE_OPTION) {
          val filepath = saveFileChooser.getSelectedFile
          lastSavePath = Some(filepath.getAbsoluteFile.getParent)
          val img = saveOptions.getSelectedItem match {
            case x: String =>
              saveOptionMap(x).map { rPixels =>
                val newGrid = mImg.g.copy(rPixels = rPixels)
                MandelImage.fromGrid(newGrid, mImg.colors)
              }.getOrElse(mImg)
            case _ => mImg
          }
          saveImage(img, filepath.getAbsolutePath)
        }
      }
    })

    colorPanel.addColumn.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val index = Integer.parseInt(colorPanel.colIndex.getText)
        val (before, after) = colorPanel.gradients.splitAt(index)
        val newPanel = ColorPanel(before ::: Gradient(0, "000000") :: after)
        setEverything(mImg)(newPanel)
      }
    })

    colorPanel.removeColumn.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val index = Integer.parseInt(colorPanel.colIndex.getText)
        val (before, after) = colorPanel.gradients.splitAt(index)
        val newPanel = ColorPanel(before ::: after.drop(1))
        setEverything(mImg)(newPanel)
      }
    })

    colorPanel.colorOptionsBox.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val selectedColors = colorPanel.colorOptionsBox.getSelectedItem
        selectedColors match {
          case s: String =>
            val newColors = MyColors.colorMap.get(s)
            newColors.foreach(c => setEverything(mImg.copy(colors = c))())
        }
      }
    })

    imgFrame.setVisible(true)
    colorFrame.setVisible(true)
  }

  setEverything(startingMandelImg)()
  HexHelper.frame.setVisible(true)

  def saveImage(mImg: MandelImage, filePath: String): Unit = {
    val infoFile = new File(s"${filePath}_info.txt")
    infoFile.createNewFile
    val infoWriter = new FileWriter(infoFile.getAbsoluteFile)
    val infoBufferWriter = new BufferedWriter(infoWriter)
    infoBufferWriter.write(mImg.g.toString)
    infoBufferWriter.write("\n")
    val writeableColors = mImg.colors
    infoBufferWriter.write(writeableColors.toString)
    infoBufferWriter.close()
    val imgFile = new File(s"$filePath.png")
    ImageIO.write(mImg.img, "png", imgFile)
  }
}
