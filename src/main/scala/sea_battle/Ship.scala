package sea_battle.ship

import sea_battle.coord.Coord
import sea_battle.orientation.Orientation
import sea_battle.orientation.Orientation.{Horizontal, Vertical}

final case class Deck(isBroken: Boolean) extends java.io.Serializable {
  def render() = if (isBroken) 'X' else '#'
}

final case class Ship(
    position: Coord,
    orientation: Orientation,
    decks: Vector[Deck]
) extends java.io.Serializable {

  def cells() = orientation match {
    case Horizontal() =>
      Vector.iterate(position, decks.length) {
        case Coord(x, y) => Coord(x + 1, y)
      }
    case Vertical() =>
      Vector.iterate(position, decks.length) {
        case Coord(x, y) => Coord(x, y + 1)
      }
  }

  def isAlive() = decks.contains(Deck(isBroken = false))

  def shot(atShot: Coord): Ship = cells.indexOf(atShot) match {
    case -1    => this
    case index => copy(decks = decks.updated(index, Deck(isBroken = true)))
  }
}

object Ship {
  def create(position: Coord, orientation: Orientation, size: Int) =
    Ship(position, orientation, Vector.fill(size)(Deck(isBroken = false)))
}

final case class Ships(ships: Vector[Ship]) extends java.io.Serializable {
  def shot(atShot: Coord) = Ships(ships.map(ship => ship.shot(atShot)))
  def allDecks() = ships.map(ship => (ship.cells.zip(ship.decks))).flatten
  def isAllBroken() = ships.forall(_.decks.forall(_ == Deck(isBroken = true)))
}

object Ships {
  def default() = Ships(Vector())
}
