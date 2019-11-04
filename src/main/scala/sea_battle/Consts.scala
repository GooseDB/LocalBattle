package sea_battle.consts

object Consts {
  val fieldSideSize = 10
  val scale = "   " ++ ('a' to 'j').foldLeft("")(_ :+ _ :+ ' ')
  val inputFormatTip = "Format: xy<[a;j][1;10]> orientation<h|v>"
  val separator = "----------------------------"
}
