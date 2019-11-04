package sea_battle.orientation

sealed trait Orientation extends java.io.Serializable

object Orientation {
  final case class Horizontal() extends Orientation
  final case class Vertical() extends Orientation

  def validate(raw: String) = raw match {
    case "h" => Right(Horizontal())
    case "v" => Right(Vertical())
    case _   => Left("Orientation is either h or v")
  }
}
