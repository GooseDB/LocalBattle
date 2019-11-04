package sea_battle.field

import sea_battle.coord.Coord
import sea_battle.cell.Cell
import sea_battle.cell.Cell.{Harmed, Unharmed}
import sea_battle.consts.Consts

final case class Field(field: Vector[Vector[Cell]]) {
  def shot(shotAt: Coord) =
    Field(field.updated(shotAt.y, field(shotAt.y).updated(shotAt.x, Harmed())))

  def cellAt(coord: Coord) = field(coord.y)(coord.x)
}

object Field {
  def default() =
    Field(Vector.fill(Consts.fieldSideSize, Consts.fieldSideSize)(Unharmed()))
}
