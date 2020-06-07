package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage

import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, FlowLayout, GridLayout}
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
  val maxIterations = 1000

  def f(z: Complex, c: Complex): Complex = z.squared + c

  // returns the number of iterations of `f` applied to `c` to yield a "large" value. returns None if still bounded
  // after 1000 iterations
  def boundedCount(c: Complex, z: Complex = Complex.zero, count: Int = 0): Option[Int] = {
    if (z.outOfBounds) {
      Some(count)
    } else if (count > maxIterations) {
      None
    } else {
      boundedCount(c, f(z, c), count + 1)
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

  val initialBaseColors = List(
    (Color.decode("#ff007f"), 4),
    (Color.decode("#9933ff"), 4),
    (Color.decode("#00BFBF"), 6),
    (Color.decode("#33FFFF"), 1)
  )
//  val initialBaseColors = List(
//    (Color.decode("#ea0303"), 2),
//    (Color.decode("#ea7d03"), 3),
//    (Color.decode("#fbf834"), 4),
//    (Color.decode("#5bfc28"), 5),
//    (Color.decode("#1919f6"), 6),
//    (Color.decode("#19dff6"), 7),
//    (Color.decode("#9933ff"), 8),
//    (Color.decode("#000000"), 1)
//  )
//  val baseColors = List(Color.decode("#02c4ff"), Color.decode("#ffd102"), Color.decode("#ff0263"))
//  val baseColors = List(Color.decode("#ff937b"), Color.decode("#7bffda"), Color.decode("#ff937b"), Color.decode("#7bffda"))
//  val baseColors = List(Color.decode("#ff007f"), Color.decode("#9933ff"), Color.decode("#ff007f"), Color.decode("#9933ff"))

  val grades = 0.to(3).map { case i => 1 - (i / 4.0)  }.reverse

//  val colorsIncludingIn = baseColors.flatMap { c => grades.map(mult(c, _)) }.map(_.getRGB)
//  val colors = colorsIncludingIn.dropRight(1)
//  val inColor = colorsIncludingIn.last

//  val inColor = Color.decode("#00FFFF").getRGB
//  val inColor = Color.decode("#FF0263").getRGB

  case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, List[(Int, Int)]], g: Grid, colors: List[(Color, Int)] = initialBaseColors) {
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
      val nonMax = pixelGroups.mapValues(_.size).toList.sortBy(_._1).filterNot(_._1 == maxIterations)
      val setupD = System.currentTimeMillis - start
      println(s"autoColor setup: $setupD millis")
      val allColors = expandBaseColors(colors)
      val notInColors = allColors.dropRight(1)
      val inColor = allColors.last
      val gro = group(notInColors.tail, nonMax, notInColors.head, nonMax.map(_._2).sum)
      //      println(gro.groupBy(_._2).mapValues(_.keys.flatMap(k => pixelGroups.getOrElse(k, Set.empty)).size).values) // this line is very expensive: 800 millis
      val duration = System.currentTimeMillis - start
      println(s"autoColors: $duration millis")
      gro + (maxIterations -> inColor)
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


  def fromGrid(g: Grid, colors: List[(Color, Int)] = initialBaseColors): MandelImage = {
    println(g)
    val start = System.currentTimeMillis
    val mirror = mirrorGate && g.iMin == 0

    val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

    val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

    def toBound(c: Complex): Int = {
      val count = boundedCount(c)
      count.getOrElse(maxIterations)
    }

    def pixelsWithIndex = {
      val withIndex = g.gridPixels.zipWithIndex
      if (g.rSize <= 4000) {
        withIndex.par
      } else {
        withIndex
      }
    }

    def bGrid = pixelsWithIndex.flatMap { case (row, h) =>
      row.zipWithIndex.map { case (c, w) => (toBound(c), (w, h)) }
    }.toList

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
    val gg = bGrid
    val bgDuration = System.currentTimeMillis - pgStart
    println(s"make bGrid: $bgDuration")
    val pixelGroups = gg.groupBy(_._1).mapValues(_.map(_._2))
    val pgDuration = System.currentTimeMillis - pgStart
    println(s"make pixelGroups: $pgDuration millis")

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
  val g = Grid(rPixels = 1200)
//  val g = Grid(rPixels = 1300, -0.566492093858939, -0.5664917813160236, 0.677928752350595, 0.6779289685008787)

  val mirrorGate = true
  val mirror = mirrorGate && g.iMin == 0

  val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

  val startingMandelImg = MyImage.fromGrid(g)
  val imgFrame = new JFrame()
  val lbl = new JLabel
  val zoomOutButton = new JButton("zoom out")
  val saveButton = new JButton("save")
  val saveHighRes = new JButton("save high res")
  val saveAs = new JTextField("my_image")
  val colorPanelWrapper = new JPanel()
  imgFrame.setLayout(new FlowLayout)
  imgFrame.setSize(g.rPixels + 10, g.iPixels + 65)
  imgFrame.add(lbl)
  imgFrame.add(zoomOutButton)
  imgFrame.add(saveButton)
  imgFrame.add(saveHighRes)
  val colorFrame = new JFrame()
  colorFrame.setLayout(new FlowLayout)
  colorFrame.setSize(500, 1200)
  colorFrame.add(colorPanelWrapper)

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd_hh-mm-ss")
  val date = new Date()
  val saveDirPath = s"$workLaptopDir/${dateFormat.format(date)}"
  val saveNum = new AtomicInteger(1)

  def fromImg(mImg: MandelImage) = {
    val colorPanelColors = mImg.colors.map { case (c, i) => (c.getRGB.toHexString.substring(2), i) }
    ColorPicker.ColorPanel(colorPanelColors)
  }

  def setEverything(mImg: MandelImage)(colorPanel: ColorPanel = fromImg(mImg)): Unit = {
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
        val newColorInput = colorPanel.columns.map { c =>
          (c.hexField.getText, Integer.parseInt(c.numField.getText))
        }
        val mImgColors = newColorInput.map { case (hex, num) => (Color.decode(s"#$hex"), num) }
        val newMImg = mImg.copy(colors = mImgColors)
        setEverything(newMImg)()
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
        saveImage(mImg)
      }
    })

    colorPanel.addColumn.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val index = Integer.parseInt(colorPanel.colIndex.getText)
        val (before, after) = colorPanel.colorInput.splitAt(index)
        val newPanel = ColorPanel(before ::: ("000000", 0) :: after)
        setEverything(mImg)(newPanel)
      }
    })

    colorPanel.removeColumn.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val index = Integer.parseInt(colorPanel.colIndex.getText)
        val (before, after) = colorPanel.colorInput.splitAt(index)
        val newPanel = ColorPanel(before ::: after.drop(1))
        setEverything(mImg)(newPanel)
      }
    })

    saveHighRes.getActionListeners.foreach(saveHighRes.removeActionListener)
    saveHighRes.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val bigGrid = mImg.g.copy(rPixels = 10000)
        val bigMimg = MyImage.fromGrid(bigGrid, mImg.colors)
        saveImage(bigMimg, "big_")
      }
    })

    imgFrame.setVisible(true)
    colorFrame.setVisible(true)
  }

  setEverything(startingMandelImg)()

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
    val writeableColors = mImg.colors.map {
      case (color, num) => (color.getRGB.toHexString.substring(2), num)
    }
    infoBufferWriter.write(writeableColors.toString)
    infoBufferWriter.close()
  }

//  val outputFile = new File(s"$personalLaptopDir/manycolor1.png")
//  ImageIO.write(img, "png", outputFile)
}
