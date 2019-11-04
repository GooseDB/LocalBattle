package sea_battle.coord

import sea_battle.consts.Consts

final case class Coord(x: Int, y: Int) extends java.io.Serializable

object Coord {
  def validate(raw: String) = {
    val (x_, y_) = (raw.head, raw.tail)

    val validatedX =
      if (('a' to 'j').contains(x_)) Right(x_ - 'a')
      else (Left("x must be ['a';'j']"))

    val validatedY = y_.toIntOption match {
      case Some(num) if (num > 0 && num <= Consts.fieldSideSize) =>
        Right(num - 1)
      case _ => Left("y must be inteeger and [1;10]")
    }

    for {
      x <- validatedX
      y <- validatedY
    } yield Coord(x, y)

  }
}
