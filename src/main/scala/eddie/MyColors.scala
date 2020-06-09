package eddie

import java.awt.Color

object MyColors {

  val candy = List(
    (Color.decode("#ff007f"), 4),
    (Color.decode("#9933ff"), 4),
    (Color.decode("#00BFBF"), 6),
    (Color.decode("#33FFFF"), 1)
  )

  val rainbow = List(
    (Color.decode("#ea0303"), 2),
    (Color.decode("#ea7d03"), 3),
    (Color.decode("#fbf834"), 4),
    (Color.decode("#5bfc28"), 5),
    (Color.decode("#1919f6"), 6),
    (Color.decode("#19dff6"), 7),
    (Color.decode("#9933ff"), 8),
    (Color.decode("#000000"), 1)
  )

  val greyscale = List(
    (Color.decode("#000000"), 20)
  )

  val ghost = List(
    (Color.decode("#cce0ff"), 12),
    (Color.decode("#353a42"), 10)
  )

}
