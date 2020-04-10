package eddie

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, FlowLayout, GridLayout}
import MandelbrotFunctions._

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
    251.to(999).map { _ -> Color.decode("#22FFFF").getRGB },
    List(1000 -> Color.decode("#99FFFF").getRGB)
  ).flatten.toMap

  case class MandelImage(img: BufferedImage, pixelGroups: Map[Int, Set[(Int, Int)]]) {
    def applyColors(colorMap: Map[Int, Int]) = {
      colorMap.foreach { case (bound, color) =>
        pixelGroups.getOrElse(bound, Set.empty).foreach { case (x, y) =>
          img.setRGB(x, y, color)
        }
      }
    }
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

    MandelImage(img, pixelGroups)
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

  val mandelImg = MyImage.fromGrid(g)

  mandelImg.applyColors(MyImage.defaultColorMap)

//  def toColor(c: Complex): Int = {
//    val count = boundedCount(c)
//    count.map(MyColors.chooseColor).getOrElse(MyColors.blue.getRGB)
//  }
//
//  def toBound(c: Complex): Int = {
//    val count = boundedCount(c)
//    count.getOrElse(1000)
//  }
//
//  def cGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
//    (row.map(toColor).zipWithIndex, h)
//  }
//
//  def bGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
//    (row.map(toBound).zipWithIndex, h)
//  }

//  val setRGB = if (mirror) {
//    (w: Int, h: Int, c: Int) => {
//      img.setRGB(w, h, c)
//      img.setRGB(w, h + ((g.iPixels - h) * 2) - 1, c)
//    }
//  } else {
//    (w: Int, h: Int, c: Int) => img.setRGB(w, h, c)
//  }

//  cGrid.foreach { case (row, h) =>
//    if (h % 1000 == 0) println(h)
//    row.foreach { case (color, w) =>
//      setRGB(w, h, color)
//    }
//  }

//  val pixelGroups = bGrid.flatMap { case (row, h) =>
//    if (h % 1000 == 0) println(h)
//    row.map { case (bound, w) =>
//      (bound, (w, h))
//    }
//  }.groupBy(_._1).mapValues(_.map(_._2).toSet).toList.sortBy(_._1)


  println()
  println(s"took: ${(System.currentTimeMillis - start) / 1000} seconds")

//  val tenColorImage = new BufferedImage(50, 500, BufferedImage.TYPE_INT_ARGB)
  val colors = List(
    "#002222",
    "#003333",
    "#004444",
    "#005555",
    "#006666",
    "#007777",
    "#008888",
    "#009999",
    "#00AAAA",
    "#00BBBB"
  ).map(Color.decode).map(_.getRGB)

  val thresholds = List(
    3,
    5,
    8,
    11,
    15,
    19,
    24,
    29,
    35,
    45,
    51,
    57,
    65,
    75,
    90,
    110,
    140,
    180,
    250,
    999,
    1000
  )

//  def partitionSets(remainingThresh: List[Int], remainingPixelSets: List[(Int, Set[(Int, Int)])], acc: List[Set[(Int, Int)]] = Nil): List[Set[(Int, Int)]] = {
//    remainingThresh match {
//      case Nil => acc
//      case h :: t =>
//        val (inSet, notInSet) = remainingPixelSets.partition(_._1 <= h)
//        val newAcc = inSet.flatMap(_._2).toSet :: acc
//        partitionSets(t, notInSet, newAcc)
//    }
//  }
//
//  // TODO: try cycling through rainbow colors
//
//  val pixelSets = partitionSets(thresholds, pixelGroups).reverse

  val img = mandelImg.img
  val icon = new ImageIcon(img)
  val frame = new JFrame

  case class ControlRow(from: JTextField, to: JTextField, color: JTextField, button: JButton) {
    def register(pixelMap: Map[Int, Set[(Int, Int)]], img: BufferedImage, lbl: JLabel): Unit = {
      button.addActionListener(new ActionListener {
        override def actionPerformed(actionEvent: ActionEvent): Unit = {
          val colorInt = Color.decode(s"#${color.getText}").getRGB
          val fromInt = Integer.parseInt(from.getText)
          val toInt = Integer.parseInt(to.getText)
          val pixels = pixelMap.filterKeys(bound => bound >= fromInt && bound <= toInt).values.flatten
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

  val controlRows = thresholds.indices.map(_ => ControlRow(new JTextField(3), new JTextField(3), new JTextField(6), new JButton("apply")))
  val pixelMap = mandelImg.pixelGroups

  val colorPanel = new JPanel()
  colorPanel.setLayout(new GridLayout(thresholds.size, 4))
  val lbl = new JLabel
  controlRows.foreach { r =>
    r.register(pixelMap, img, lbl)
    r.addToPanel(colorPanel)
  }
  frame.setLayout(new FlowLayout)
  frame.setSize(4000, 2500)
  lbl.setIcon(icon)
  frame.add(lbl)
  frame.add(colorPanel)
  frame.setVisible(true)

//  val outputFile = new File(s"$personalLaptopDir/manycolor1.png")
//  ImageIO.write(img, "png", outputFile)
}

class ButtonListener(pixels: List[(Int, Int)], img: BufferedImage, lbl: JLabel, txt: JTextField) extends ActionListener {
  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    val color = Color.decode(s"#${txt.getText}").getRGB
    pixels.foreach { case (x, y) => img.setRGB(x, y, color) }
    lbl.setIcon(new ImageIcon(img))
  }
}
