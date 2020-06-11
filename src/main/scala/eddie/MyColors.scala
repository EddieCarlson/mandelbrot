package eddie

object MyColors {
  val candy = List(
    Gradient(4, "ff007f"),
    Gradient(4, "9933ff"),
    Gradient(6, "00BFBF"),
    Gradient(1, "33FFFF")
  )

  val darkRainbow = List(
    Gradient(2, "ea0303"),
    Gradient(3, "ea7d03"),
    Gradient(4, "fbf834"),
    Gradient(5, "5bfc28"),
    Gradient(6, "1919f6"),
    Gradient(7, "19dff6"),
    Gradient(8, "9933ff"),
    Gradient(1, "000000")
  )

  val lightRainbow = List(
    Gradient(3,"ea7d03","ea0303"),
    Gradient(4,"fbf834","ea7d03"),
    Gradient(4,"5bfc28","fbf834"),
    Gradient(6,"1919f6","5bfc28"),
    Gradient(7,"19dff6","1919f6"),
    Gradient(7,"9933ff","19dff6"),
    Gradient(0,"9933ff","000000"),
    Gradient(1,"ffffff","000000")
  )

  val greyscale = List(
    Gradient(20, "000000", "ffffff")
  )

  val ghost = List(
    Gradient(12, "cce0ff"),
    Gradient(10, "353a42")
  )

  val snowflake = List(
    Gradient(12,"61adbc","ffffff",true,true),
    Gradient(4,"ffffff","61adbc",true,true)
  )

  val glacier = List(
    Gradient(15,"ffffff","115555",true,true),
    Gradient(7,"157777","ffffff",true,true)
  )

  val colors = List(
    "candy" -> candy,
    "darkRainbow" -> darkRainbow,
    "lightRainbow" -> lightRainbow,
    "greyscale" -> greyscale,
    "ghost" -> ghost,
    "snowflake" -> snowflake,
    "glacier" -> glacier
  )

  val colorMap = colors.toMap
  val reverseMap = colors.map(_.swap).toMap
}
