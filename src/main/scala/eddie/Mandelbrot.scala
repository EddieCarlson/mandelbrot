package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, FlowLayout, GridLayout}

import MandelbrotFunctions._
import eddie.MyImage.MandelImage

object MandelbrotFunctions {
  def f(z: Complex, c: Complex): Complex = z.squared + c

  // returns the number of iterations of `f` applied to `c` to yield a "large" value. returns None if still bounded
  // after 1000 iterations
  def boundedCount(c: Complex, z: Complex = Complex.zero, count: Int = 0): Option[Int] = {
    if (z.outOfBounds) {
      Some(count)
    } else if (count > 1000) {
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

  val defaultColorMap = List(
    1.to(2).map { _ -> Color.decode("#000000").getRGB },
    3.to(4).map { _ -> Color.decode("#160009").getRGB },
    5.to(6).map { _ -> Color.decode("#330019").getRGB },
    7.to(10).map { _ -> Color.decode("#99004c").getRGB },
    11.to(15).map { _ -> Color.decode("#cc0066").getRGB },
    16.to(20).map { _ -> Color.decode("#ff007f").getRGB },
    21.to(25).map { _ -> Color.decode("#330066").getRGB },
    26.to(30).map { _ -> Color.decode("#4c0099").getRGB },
    31.to(35).map { _ -> Color.decode("#6600cc").getRGB },
    36.to(40).map { _ -> Color.decode("#7f00ff").getRGB },
    41.to(45).map { _ -> Color.decode("#9933ff").getRGB },
    46.to(60).map { _ -> Color.decode("#006666").getRGB },
    61.to(80).map { _ -> Color.decode("#009999").getRGB },
    81.to(120).map { _ -> Color.decode("#00CCCC").getRGB },
    121.to(250).map { _ -> Color.decode("#00FFFF").getRGB },
    251.to(999).map { _ -> Color.decode("#00FFFF").getRGB },
    List(1000 -> Color.decode("#99FFFF").getRGB)
  ).flatten.toMap

  // TODO: don't store grid
  case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, Set[(Int, Int)]], g: Grid) {
    def applyColors(colorMap: Map[Int, Int]) = {
      colorMap.foreach { case (bound, color) =>
        pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
          img.setRGB(x, y, color)
        }
      }
    }
    // TODO: this isn't good enough - not all thresholds are represented, don't get a full coloring
    def extractColorMap: Map[Int, Int] = {
      pixelGroups.mapValues(_.toList).collect {
        case (bound, (x, y) :: _) => bound -> img.getRGB(x, y)
      }
    }
  }

  def fromGrid(g: Grid): MandelImage = {
    val mirror = mirrorGate && g.iMin == 0

    val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

    val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

    def toBound(c: Complex): Int = {
      val count = boundedCount(c)
      count.getOrElse(1000)
    }

    def bGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
      (row.map(toBound).zipWithIndex, h)
    }

    // TODO: does mirroring still work?
//    val setRGB = if (mirror) {
//      (w: Int, h: Int, c: Int) => {
//        img.setRGB(w, h, c)
//        img.setRGB(w, h + ((g.iPixels - h) * 2) - 1, c)
//      }
//    } else {
//      (w: Int, h: Int, c: Int) => img.setRGB(w, h, c)
//    }

    val pixelGroups = bGrid.flatMap { case (row, h) =>
      if (h % 1000 == 0) println(h)
      row.map { case (bound, w) =>
        (bound, (w, h))
      }
    }.groupBy(_._1).mapValues(_.map(_._2).toSet)

    MandelImage(img, pixelGroups, g)
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
  val g = new Grid(rPixels = 1300, rMin = -1.0704, rMax = -0.41495, iMin = 0.4063, iMax = 0.8596)
//  val g = Grid(rPixels = 1300)

  val mirrorGate = true
  val mirror = mirrorGate && g.iMin == 0

  val imgYPixels = if (mirror) g.iPixels * 2 else g.iPixels

  //  val img = new BufferedImage(g.rPixels, g.iPixels, BufferedImage.TYPE_INT_ARGB)
//  val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

  var curMandelImg = MyImage.fromGrid(g)

  val thresholds = List(1,2,3,4,5,6,7,8)
  val controlRows = thresholds.indices.map(_ => ControlRow(new JTextField(3), new JTextField(3), new JTextField(6), new JButton("apply")))

//  val img = mandelImg.img
//  val icon = new ImageIcon(img)
  val frame = new JFrame()
  val colorPanel = new JPanel()
  colorPanel.setLayout(new GridLayout(thresholds.size, 4))
  val lbl = new JLabel

  def setImg(mandelImg: MandelImage, colorMap: Map[Int, Int] = MyImage.defaultColorMap) = {
    curMandelImg = mandelImg
    mandelImg.applyColors(colorMap)
    lbl.setIcon(new ImageIcon(mandelImg.img))
  }

  lbl.addMouseListener(new MouseListener {
    override def mouseClicked(mouseEvent: MouseEvent): Unit = {
      val point = mouseEvent.getPoint
      setImg(MyImage.fromGrid(curMandelImg.g.zoomCenteredOn(point.x, point.y)), curMandelImg.extractColorMap)
    }

    override def mousePressed(mouseEvent: MouseEvent): Unit = {}

    override def mouseReleased(mouseEvent: MouseEvent): Unit = {}

    override def mouseEntered(mouseEvent: MouseEvent): Unit = {}

    override def mouseExited(mouseEvent: MouseEvent): Unit = {}
  })

  controlRows.foreach { r =>
    r.register(curMandelImg, lbl)
    r.addToPanel(colorPanel)
  }
  frame.setLayout(new FlowLayout)
  frame.setSize(4000, 2500)

  setImg(curMandelImg)

  frame.add(lbl)
  frame.add(colorPanel)
  frame.setVisible(true)

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


class ButtonListener(pixels: List[(Int, Int)], img: BufferedImage, lbl: JLabel, txt: JTextField) extends ActionListener {
  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    val color = Color.decode(s"#${txt.getText}").getRGB
    pixels.foreach { case (x, y) => img.setRGB(x, y, color) }
    lbl.setIcon(new ImageIcon(img))
  }
}
