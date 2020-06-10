package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage

import javax.swing.{ImageIcon, JButton, JComboBox, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, Dimension, FlowLayout, GridLayout}
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger

import MandelbrotFunctions._
import eddie.MyImage.MandelImage
import scala.collection.mutable
import scala.collection.JavaConverters._

import eddie.ColorPicker.ColorPanel
import javax.imageio.ImageIO

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

object MyImage {
  val mirrorGate = true
    val reds = List(
      ("#160009", 4),
      ("#330019", 6),
      ("#99004c", 10),
      ("#cc0066", 15),
      ("#ff007f", 20)
    )

    val purps = List(
      ("#330066", 25),
      ("#4c0099", 30),
      ("#6600cc", 35),
      ("#7f00ff", 40),
      ("#9933ff", 45)
    )

    val blues = List(
      ("#006666", 60),
      ("#009999", 80),
      ("#00CCCC", 120),
      ("#00FFFF", 250),
    )

  def mult(c: Color, f: Double): Color = {
    new Color((c.getRed * f).toInt, (c.getGreen * f).toInt, (c.getBlue * f).toInt)
  }

  type ColorInt = Int

  def expandBaseColors(baseColors: List[(Color, Int)]): List[ColorInt] = {
    baseColors.flatMap { case (color, num) =>
      val multipliers = 0.until(num).map { i => 1 - (i.toDouble / num) }.reverse
      multipliers.map(mult(color, _)).map(_.getRGB)
    }
  }

  val initialBaseColors = MyColors.candy

case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, List[(Int, Int)]], g: Grid, colors: List[Gradient] = initialBaseColors.map(Function.tupled(Gradient.fromColor))) {
  val autoColors: Map[Int, ColorInt] = {
    def group(remainingColors: List[Int], remainingGroups: List[(Int, Int)], curColor: Int, remainingPixelCount: Int, curPixels: Int = 0, acc: Map[Int, Int] = Map.empty): Map[Int, Int] = {
      if (remainingGroups.isEmpty) {
        acc
      } else if (remainingColors.isEmpty) {
        val remainingMap = remainingGroups.map(_._1).map(t => (t, curColor)).toMap
        acc ++ remainingMap
      } else if (curPixels.toDouble / remainingPixelCount < 1.0 / remainingColors.size.toDouble) {
        val (toAdd, size) :: tail = remainingGroups
        group(remainingColors, tail, curColor, remainingPixelCount - size, curPixels + size, acc + (toAdd -> curColor))
      } else {
        val nextColor :: tail = remainingColors
        group(tail, remainingGroups, nextColor, remainingPixelCount, 0, acc)
      }
    }
    val start = System.currentTimeMillis
    val nonMax = pixelGroups.mapValues(_.size).toList.sortBy(_._1).filterNot(_._1 == g.maxIterations)
    val setupD = System.currentTimeMillis - start
    println(s"autoColor setup: $setupD millis")
    val allColors = colors.flatMap(_.gradations)
    val notInColors = allColors.dropRight(1)
    val inColor = allColors.last
    val gro = group(notInColors.tail, nonMax, notInColors.head, nonMax.map(_._2).sum)
    //      println(gro.groupBy(_._2).mapValues(_.keys.flatMap(k => pixelGroups.getOrElse(k, Set.empty)).size).values) // this line is very expensive: 800 millis
    val duration = System.currentTimeMillis - start
    println(s"autoColors: $duration millis")
    gro + (g.maxIterations -> inColor)
  }

  val start = System.currentTimeMillis
  autoColors.foreach { case (bound, color) =>
    pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
      img.setRGB(x, y, color)
    }
  }
  val duration = System.currentTimeMillis - start
  println(s"applyColors: $duration millis")
}


  def fromGrid(g: Grid, colors: List[Gradient] = initialBaseColors.map(Function.tupled(Gradient.fromColor))): MandelImage = {
    println(g)
    val start = System.currentTimeMillis
    val mirror = mirrorGate && g.iMin == 0

    val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

    val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

    // TODO: does mirroring still work?
//    val setRGB = if (mirror) {
//      (w: Int, h: Int, c: Int) => {
//        img.setRGB(w, h, c)
//        img.setRGB(w, h + ((g.iPixels - h) * 2) - 1, c)
//      }
//    } else {
//      (w: Int, h: Int, c: Int) => img.setRGB(w, h, c)
//    }

    val pgStart = System.currentTimeMillis
    val pixelGroups = g.escapeBounds.groupBy(_._1).mapValues(_.map(_._2).toList)
    val pgDuration = System.currentTimeMillis - pgStart
    println(s"escape bounds and pixelGroups: $pgDuration millis")

    val mi = MandelImage(img, pixelGroups, g, colors)
    val duration = System.currentTimeMillis - start
    println(s"fromGrid: $duration millis")
    mi
  }
}

object Mandelbrot extends App {
  val start = System.currentTimeMillis

  val personalLaptopDir = "/Users/eddie/IdeaProjects/mandelbrot"
  val workLaptopDir = "/Users/eddie.carlson/developer/eddie/mandelbrot/explorer_images"

  //  val g = new HalfGrid(20000)
  //  val g = new Grid(900)

//  val g = new Grid(rPixels = 120, rMin = 1.16 - 2, rMax = 1.30 - 2, iMin = 1.0356 - 1, iMax = 1.259 - 1)
//    val g = new Grid(rPixels = 15000, rMin = -0.6704, rMax = -0.41495, iMin = 0.5063, iMax = 0.7196)
//  val g = new Grid(rPixels = 1300, rMin = -1.0704, rMax = -0.41495, iMin = 0.4063, iMax = 0.8596)
  val g = Grid(rPixels = 1000)
//  val g = Grid(rPixels = 1300, -0.566492093858939, -0.5664917813160236, 0.677928752350595, 0.6779289685008787)

  val mirrorGate = true
  val mirror = mirrorGate && g.iMin == 0

  val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

  val startingMandelImg = MyImage.fromGrid(g)
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
  val saveDirPath = s"$workLaptopDir/${dateFormat.format(date)}"
  val saveNum = new AtomicInteger(1)

  def setEverything(mImg: MandelImage)(colorPanel: ColorPanel = ColorPanel(mImg.colors)): Unit = {
    lbl.setIcon(new ImageIcon(mImg.img))
    colorPanelWrapper.removeAll()
    colorPanelWrapper.add(colorPanel.panel)

    lbl.getMouseListeners.foreach(lbl.removeMouseListener)
    lbl.addMouseListener(new MouseListener {
      override def mouseClicked(mouseEvent: MouseEvent): Unit = {
        val point = mouseEvent.getPoint
        val newGrid = mImg.g.zoomCenteredOn(point.x, point.y)
        val newMImg = MyImage.fromGrid(newGrid, mImg.colors)
        setEverything(newMImg)()
      }

      override def mousePressed(mouseEvent: MouseEvent): Unit = {}
      override def mouseReleased(mouseEvent: MouseEvent): Unit = {}
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
        setEverything(MyImage.fromGrid(newGrid, mImg.colors))()
      }
    })

    iterationButton.getActionListeners.foreach(iterationButton.removeActionListener)
    iterationButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val newGrid = mImg.g.copy(maxIterations = Integer.parseInt(iterationField.getText))
        setEverything(MyImage.fromGrid(newGrid, mImg.colors))()
      }
    })

    zoomOutButton.getActionListeners.foreach(zoomOutButton.removeActionListener)
    zoomOutButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        setEverything(MyImage.fromGrid(mImg.g.zoomOut, mImg.colors))()
      }
    })

    saveButton.getActionListeners.foreach(saveButton.removeActionListener)
    saveButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val img = saveOptions.getSelectedItem match {
          case x: String =>
            saveOptionMap(x).map { rPixels =>
              val newGrid = mImg.g.copy(rPixels = rPixels)
              MyImage.fromGrid(newGrid, mImg.colors)
            }.getOrElse(mImg)
        }
        saveImage(img)
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

    imgFrame.setVisible(true)
    colorFrame.setVisible(true)
  }

  setEverything(startingMandelImg)()
  HexHelper.frame.setVisible(true)

  def saveImage(mImg: MandelImage, filePrefix: String = ""): Unit = {
    val dir = new File(saveDirPath)
    if (!dir.exists) dir.mkdir
    val saveNumPath = s"$saveDirPath/$filePrefix${saveNum.getAndIncrement}"
    val imgFile = new File(s"$saveNumPath.png")
    ImageIO.write(mImg.img, "png", imgFile)
    val infoFile = new File(s"${saveNumPath}_info.txt")
    infoFile.createNewFile
    val infoWriter = new FileWriter(infoFile.getAbsoluteFile)
    val infoBufferWriter = new BufferedWriter(infoWriter)
    infoBufferWriter.write(mImg.g.toString)
    infoBufferWriter.write("\n")
    val writeableColors = mImg.colors
    infoBufferWriter.write(writeableColors.toString)
    infoBufferWriter.close()
  }

//  val outputFile = new File(s"$personalLaptopDir/manycolor1.png")
//  ImageIO.write(img, "png", outputFile)
}
