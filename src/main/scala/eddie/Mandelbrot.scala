package eddie

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField}
import java.awt.{Color, FlowLayout, GridLayout}

object Mandelbrot extends App {
  val start = System.currentTimeMillis

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
  val img = new BufferedImage(g.rPixels, imgYPixels, BufferedImage.TYPE_INT_ARGB)

  def toColor(c: Complex): Int = {
    val count = boundedCount(c)
    count.map(MyColors.chooseColor).getOrElse(MyColors.blue.getRGB)
  }

  def toBound(c: Complex): Int = {
    val count = boundedCount(c)
    count.getOrElse(1000)
  }

  def cGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
    (row.map(toColor).zipWithIndex, h)
  }

  def bGrid = g.gridPixels.zipWithIndex.map { case (row, h) =>
    (row.map(toBound).zipWithIndex, h)
  }

  val setRGB = if (mirror) {
    (w: Int, h: Int, c: Int) => {
      img.setRGB(w, h, c)
      img.setRGB(w, h + ((g.iPixels - h) * 2) - 1, c)
    }
  } else {
    (w: Int, h: Int, c: Int) => img.setRGB(w, h, c)
  }

  cGrid.foreach { case (row, h) =>
    if (h % 1000 == 0) println(h)
    row.foreach { case (color, w) =>
      setRGB(w, h, color)
    }
  }

  val pixelGroups = bGrid.flatMap { case (row, h) =>
    if (h % 1000 == 0) println(h)
    row.map { case (bound, w) =>
      (bound, (w, h))
    }
  }.groupBy(_._1).mapValues(_.map(_._2).toSet).toList.sortBy(_._1)


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
  def partitionSets(remainingThresh: List[Int], remainingPixelSets: List[(Int, Set[(Int, Int)])], acc: List[Set[(Int, Int)]] = Nil): List[Set[(Int, Int)]] = {
    remainingThresh match {
      case Nil => acc
      case h :: t =>
        val (inSet, notInSet) = remainingPixelSets.partition(_._1 <= h)
        val newAcc = inSet.flatMap(_._2).toSet :: acc
        partitionSets(t, notInSet, newAcc)
    }
  }

  val pixelSets = partitionSets(thresholds, pixelGroups).reverse


//  val pixelMap = colors.zipWithIndex.flatMap { case (color, imgNum) =>
//    0.to(49).flatMap { x =>
//      0.to(49).map { internalY =>
//        val y = internalY + (imgNum * 50)
//        tenColorImage.setRGB(x, y, color)
//        (imgNum, (x, y))
//      }
//    }
//  }.groupBy(_._1).mapValues(_.map(_._2))

  val icon = new ImageIcon(img)
  val frame = new JFrame

  val colorPanel = new JPanel()
  colorPanel.setLayout(new GridLayout(thresholds.size, 3))
  val components = thresholds.indices.map(i => new JButton("set color") -> (new JTextField(6), i))
  val thresholdLabels = thresholds.map(t => new JLabel(s"- to $t"))
  components.zip(thresholdLabels).foreach { case ((butt, (txt, _)), thresh) => colorPanel.add(thresh); colorPanel.add(txt); colorPanel.add(butt) }
  val buttMap = components.toMap.mapValues { case (txt, i) => (txt, pixelSets(i).toList) }
  buttMap.values.foreach { case (txt, (x, y) :: _) => txt.setText(img.getRGB(x, y).toHexString.substring(2)) }
  val lbl = new JLabel
  buttMap.foreach { case (butt, (txt, pixels)) =>
    butt.addActionListener(new ButtonListener(pixels, img, lbl, txt))
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
