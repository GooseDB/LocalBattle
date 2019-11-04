package sea_battle.cell

sealed trait Cell {
  def render() = this match {
    case Cell.Unharmed() => '_'
    case Cell.Harmed()   => '.'
  }
}
object Cell {
  final case class Unharmed() extends Cell
  final case class Harmed() extends Cell
}
