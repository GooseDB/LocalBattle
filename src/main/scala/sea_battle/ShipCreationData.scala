package sea_battle.ship_creation_data

import sea_battle.coord.Coord
import sea_battle.consts.Consts
import sea_battle.ship.Ship
import sea_battle.orientation.Orientation.{Horizontal, Vertical}
import sea_battle.orientation.Orientation

final case class ShipCreationData(
    position: Coord,
    orientation: Orientation,
    size: Int
) {
  def validate(ships: Vector[Ship]): Either[String, Ship] = {

    def withinField: Coord => Boolean = {
      case Coord(x, y) =>
        x >= 0 && x < Consts.fieldSideSize && y >= 0 && y < Consts.fieldSideSize
    }

    def shiftCell(coord: Coord, dx: Int, dy: Int): Coord =
      Coord(coord.x + dx, coord.y + dy)

    def buildShitedCells(coord: Coord, d: Vector[(Int, Int)]): Vector[Coord] =
      d.map(d => shiftCell(coord, d._1, d._2))

    lazy val shipCells =
      Ship.create(this.position, this.orientation, this.size).cells

    lazy val shipIsWithinField = shipCells.forall(withinField(_))

    lazy val shipDoesNotCrossOthers = shipCells.zipWithIndex.forall { v =>
      val (coord, index) = v
      val dcoords1 = if (index == 0) orientation match {
        case Vertical()   => Vector((-1, -1), (0, -1), (1, -1))
        case Horizontal() => Vector((-1, -1), (-1, 0), (-1, 1))
      } else Vector[(Int, Int)]()
      val dcoords2 = if (index + 1 == size) orientation match {
        case Vertical()   => Vector((-1, 1), (0, 1), (1, 1))
        case Horizontal() => Vector((1, -1), (1, 0), (1, 1))
      } else Vector[(Int,Int)]()
      val dcoords3 = orientation match {
        case Horizontal() => Vector((0, -1), (0, 1))
        case Vertical()   => Vector((-1, 0), (1, 0))
      }
      val shifted = buildShitedCells(coord, dcoords1 ++ dcoords2 ++ dcoords3)

      ships.map(_.cells).flatten.intersect(shifted :+ this.position).isEmpty
    }
    if (shipIsWithinField && shipDoesNotCrossOthers)
      Right(Ship.create(position, orientation, size))
    else Left("Ship can't stay here")
  }
}

object ShipCreationData {
  @scala.annotation.tailrec
  def createShip(
      ships: Vector[Ship],
      size: Int
  ): Ship = {
    def validateLength: Vector[String] => Either[String, Vector[String]] =
      input => if (input.length == 2) Right(input) else Left("Wrong format")

    def validateFormat: Vector[String] => Either[String, ShipCreationData] = {
      case Vector(rawC, rawO) => {
        for {
          coord <- Coord.validate(rawC)
          orientation <- Orientation.validate(rawO)
        } yield ShipCreationData(coord, orientation, size)
      }
    }

    val input = scala.io.StdIn.readLine.split(" ").toVector.filter(_ != "")

    val result = validateLength(input)
      .flatMap(validateFormat(_))
      .flatMap(_.validate(ships))

    result match {
      case Right(data) => data
      case Left(err) => {
        println(s"$err. Try again")
        createShip(ships, size)
      }
    }

  }
}
