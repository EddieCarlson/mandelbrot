package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage

import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, FlowLayout, GridLayout}

import MandelbrotFunctions._
import eddie.MyImage.MandelImage
import scala.collection.mutable
import scala.collection.JavaConverters._

import eddie.ColorPicker.ColorPanel

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

  val initialBaseColors = List((Color.decode("#ff007f"), 4), (Color.decode("#9933ff"), 4), (Color.decode("#00BFBF"), 4))
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
    def autoColors(baseColors: List[(Color, Int)] = initialBaseColors): Map[Int, ColorInt] = {
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
      val allColors = expandBaseColors(baseColors)
      val colors = allColors.dropRight(1)
      val inColor = allColors.last
      val gro = group(colors.tail, nonMax, colors.head, nonMax.map(_._2).sum)
      //      println(gro.groupBy(_._2).mapValues(_.keys.flatMap(k => pixelGroups.getOrElse(k, Set.empty)).size).values) // this line is very expensive: 800 millis
      val duration = System.currentTimeMillis - start
      println(s"autoColors: $duration millis")
      gro + (maxIterations -> inColor)
    }

    val start = System.currentTimeMillis
    autoColors(colors).foreach { case (bound, color) =>
      pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
        img.setRGB(x, y, color)
      }
    }
    val duration = System.currentTimeMillis - start
    println(s"applyColors: $duration millis")
  }

  // TODO: don't store grid
//  case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, List[(Int, Int)]], g: Grid, initialBaseColors: List[(Color, Int)] = initialBaseColors) {
//    // map of bound to color
//    def autoColors(baseColors: List[(Color, Int)] = initialBaseColors): Map[Int, ColorInt] = {
//      def group(remainingColors: List[Int], remainingGroups: List[(Int, Int)], curColor: Int, remainingPixelCount: Int, curPixels: Int = 0, acc: Map[Int, Int] = Map.empty): Map[Int, Int] = {
//        if (remainingGroups.isEmpty) {
//          acc
//        } else if (remainingColors.isEmpty) {
//          val remainingMap = remainingGroups.map(_._1).map(t => (t, curColor)).toMap
//          acc ++ remainingMap
//        } else if (curPixels.toDouble / remainingPixelCount < 1.0 / remainingColors.size.toDouble) {
//          val (toAdd, size) :: tail = remainingGroups
//          group(remainingColors, tail, curColor, remainingPixelCount - size, curPixels + size, acc + (toAdd -> curColor))
//        } else {
//          val nextColor :: tail = remainingColors
//          group(tail, remainingGroups, nextColor, remainingPixelCount, 0, acc)
//        }
//      }
//      val start = System.currentTimeMillis
//      val nonMax = pixelGroups.mapValues(_.size).toList.sortBy(_._1).filterNot(_._1 == maxIterations)
//      val setupD = System.currentTimeMillis - start
//      println(s"autoColor setup: $setupD millis")
//      val allColors = expandBaseColors(baseColors)
//      val colors = allColors.dropRight(1)
//      val inColor = allColors.last
//      val gro = group(colors.tail, nonMax, colors.head, nonMax.map(_._2).sum)
//      //      println(gro.groupBy(_._2).mapValues(_.keys.flatMap(k => pixelGroups.getOrElse(k, Set.empty)).size).values) // this line is very expensive: 800 millis
//      val duration = System.currentTimeMillis - start
//      println(s"autoColors: $duration millis")
//      gro + (maxIterations -> inColor)
//    }
//
//    def applyColors(colorMap: Map[Int, Int]) = {
//      val start = System.currentTimeMillis
//      colorMap.foreach { case (bound, color) =>
//        pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
//          img.setRGB(x, y, color)
//        }
//      }
//      val duration = System.currentTimeMillis - start
//      println(s"applyColors: $duration millis")
//    }
//    // TODO: this isn't good enough - not all thresholds are represented, don't get a full coloring
//    def extractColorMap: Map[Int, Int] = {
//      pixelGroups.mapValues(_.toList).collect {
//        case (bound, (x, y) :: _) => bound -> img.getRGB(x, y)
//      }
//    }
//  }

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

    def bGrid = g.gridPixels.zipWithIndex.par.flatMap { case (row, h) =>
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
  val workLaptopDir = "/Users/eddie.carlson/developer/eddie/mandelbrot"

  //  val g = new HalfGrid(20000)
  //  val g = new Grid(900)

//  val g = new Grid(rPixels = 120, rMin = 1.16 - 2, rMax = 1.30 - 2, iMin = 1.0356 - 1, iMax = 1.259 - 1)
//    val g = new Grid(rPixels = 15000, rMin = -0.6704, rMax = -0.41495, iMin = 0.5063, iMax = 0.7196)
//  val g = new Grid(rPixels = 1300, rMin = -1.0704, rMax = -0.41495, iMin = 0.4063, iMax = 0.8596)
  val g = Grid(rPixels = 1300)
//  val g = Grid(rPixels = 1300, -0.566492093858939, -0.5664917813160236, 0.677928752350595, 0.6779289685008787)

  val mirrorGate = true
  val mirror = mirrorGate && g.iMin == 0

  val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

  //  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)
//  val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

//  var curMandelImg = MyImage.fromGrid(g)

//  val colorCount = MyImage.colors.size + 1
//  val controlRows = 1.to(colorCount).map(_ => ControlRow(new JTextField(3), new JTextField(3), new JTextField(6), new JButton("apply")))

//  val img = mandelImg.img
//  val icon = new ImageIcon(img)
  val startingMandelImg = MyImage.fromGrid(g)
  val frame = new JFrame()
  val lbl = new JLabel
  val colorPanelWrapper = new JPanel()
  frame.setLayout(new FlowLayout)
  frame.setSize(2000, 1500)
  frame.add(lbl)
  frame.add(colorPanelWrapper)

  def setEverything(mImg: MandelImage): Unit = {
    lbl.getMouseListeners.foreach(l => lbl.removeMouseListener(l))
    lbl.setIcon(new ImageIcon(mImg.img))
    val colorPanelColors = mImg.colors.map { case (c, i) => (c.getRGB.toHexString.substring(2), i) }
    val colorPanel = ColorPicker.ColorPanel(colorPanelColors)
    colorPanelWrapper.removeAll()
    colorPanelWrapper.add(colorPanel.panel)

    lbl.addMouseListener(new MouseListener {
      override def mouseClicked(mouseEvent: MouseEvent): Unit = {
        val point = mouseEvent.getPoint
        val newGrid = mImg.g.zoomCenteredOn(point.x, point.y)
        val newMImg = MyImage.fromGrid(newGrid, mImg.colors)
        setEverything(newMImg)
      }

      override def mousePressed(mouseEvent: MouseEvent): Unit = {}
      override def mouseReleased(mouseEvent: MouseEvent): Unit = {}
      override def mouseEntered(mouseEvent: MouseEvent): Unit = {}
      override def mouseExited(mouseEvent: MouseEvent): Unit = {}
    })

    colorPanel.changeColorsButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val newColorInput = colorPanel.columns.map { c =>
          println(c.hexField.getText)
          (c.hexField.getText, Integer.parseInt(c.numField.getText))
        }
        val mImgColors = newColorInput.map { case (hex, num) => (Color.decode(s"#$hex"), num) }
        val newMImg = mImg.copy(colors = mImgColors)
        setEverything(newMImg)
      }
    })

    frame.setVisible(true)
  }

  setEverything(startingMandelImg)

//  colorPanel.setLayout(new GridLayout(curMandelImg.baseColors.size + 1, 2))
//  curMandelImg.baseColors.map { _map => (new JTextField(6), new JTextField(2)) }
//  colorPanel.setLayout(new GridLayout(colorCount + 1, 4))

//  def setImg(mandelImg: MandelImage) = {
//    curMandelImg = mandelImg
//    mandelImg.applyColors(mandelImg.autoColors(MyImage.initialBaseColors))
//    lbl.setIcon(new ImageIcon(mandelImg.img))
//  }
//
//  lbl.addMouseListener(new MouseListener {
//    override def mouseClicked(mouseEvent: MouseEvent): Unit = {
//      val point = mouseEvent.getPoint
//      val start = System.currentTimeMillis
//      val newImg = curMandelImg.g.zoomCenteredOn(point.x, point.y)
//      val gridDuration = System.currentTimeMillis - start
//      println(s"grid build: $gridDuration millis")
//      setImg(MyImage.fromGrid(newImg))
//      val duration = System.currentTimeMillis - start
//      println(s"full time: $duration millis")
//    }
//
//    override def mousePressed(mouseEvent: MouseEvent): Unit = {}
//    override def mouseReleased(mouseEvent: MouseEvent): Unit = {}
//    override def mouseEntered(mouseEvent: MouseEvent): Unit = {}
//    override def mouseExited(mouseEvent: MouseEvent): Unit = {}
//  })

//  controlRows.foreach { r =>
//    r.register(curMandelImg, lbl)
//    r.addToPanel(colorPanel)
//  }
  val zoomOutButton = new JButton("out")
//  colorPanel.add(zoomOutButton)
//  zoomOutButton.addActionListener(new ActionListener {
//    override def actionPerformed(actionEvent: ActionEvent): Unit = {
//      setImg(MyImage.fromGrid(curMandelImg.g.zoomOut))
//    }
//  })
//  val loadColors = new JButton("get")
//  loadColors.addActionListener(new ActionListener {
//    override def actionPerformed(actionEvent: ActionEvent): Unit = {
//      // list of color to range of pixels
//      val ranges = curMandelImg.extractColorMap.toList.groupBy(_._2).mapValues(_.map(_._1).sorted).toList.sortBy(_._2.head).map(x => (x._1, (x._2.head, x._2.last)))
//      ranges.zip(controlRows).foreach { case ((color, (lower, upper)),row ) =>
//        row.from.setText(lower.toString)
//        row.to.setText(upper.toString)
//        row.color.setText(color.toHexString.substring(2))
//      }
//    }
//  })
//  colorPanel.add(loadColors)

//  val colorPanelColors = MyImage.initialBaseColors.map { case (c, i) => (c.getRGB.toHexString.substring(2), i) }
//  val realColorPanel = ColorPicker.ColorPanel(colorPanelColors, colorPanel, frame, curMandelImg, lbl)
//  colorPanel.add(realColorPanel.panel)
//
//
//  setImg(curMandelImg)
//
//  frame.add(lbl)
//  frame.add(colorPanel)
//  frame.setVisible(true)

//  val outputFile = new File(s"$personalLaptopDir/manycolor1.png")
//  ImageIO.write(img, "png", outputFile)
}

case class ControlRow(from: JTextField, to: JTextField, color: JTextField, button: JButton) {
  def register(mImg: => MandelImage, lbl: JLabel): Unit = {
    button.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        val colorInt = Color.decode(s"#${color.getText}").getRGB
        val fromInt = Integer.parseInt(from.getText)
        val toInt = Integer.parseInt(to.getText)
        val pixels = mImg.pixelGroups.filterKeys(bound => bound >= fromInt && bound <= toInt).values.flatten
        val img = mImg.img
        pixels.foreach { case (x, y) => img.setRGB(x, y, colorInt) }
        lbl.setIcon(new ImageIcon(img))
      }
    })
  }
  def addToPanel(panel: JPanel): Unit = {
    panel.add(from)
    panel.add(to)
    panel.add(color)
    panel.add(button)
  }
}


//class ButtonListener(pixels: List[(Int, Int)], img: BufferedImage, lbl: JLabel, txt: JTextField) extends ActionListener {
//  override def actionPerformed(actionEvent: ActionEvent): Unit = {
//    val color = Color.decode(s"#${txt.getText}").getRGB
//    pixels.foreach { case (x, y) => img.setRGB(x, y, color) }
//    lbl.setIcon(new ImageIcon(img))
//  }
//}
