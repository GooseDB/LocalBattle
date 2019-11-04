package sea_battle.entities

import sea_battle.entities.Cell._
import sea_battle.entities.Orientation._
import sea_battle.consts.Consts
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

final case class Coord(x: Int, y: Int) extends java.io.Serializable

final case class GameOver(gameOver: Boolean) extends java.io.Serializable

object GameOver {
  implicit def gameOverToBoolean(gameOver: GameOver) = gameOver.gameOver
  implicit def BooleantoGameOver(gameOver: Boolean) = GameOver(gameOver)
}

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

sealed trait Cell {
  def render() = this match {
    case Unharmed() => '_'
    case Harmed()   => '.'
  }
}
object Cell {
  final case class Unharmed() extends Cell
  final case class Harmed() extends Cell
}

final case class Deck(isBroken: Boolean) extends java.io.Serializable {
  def render() = if (isBroken) 'X' else '#'
}

final case class Ship(
    position: Coord,
    orientation: Orientation,
    decks: Array[Deck]
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
    Ship(position, orientation, Array.fill(size)(Deck(isBroken = false)))
}

final case class Ships(ships: Array[Ship]) extends java.io.Serializable {
  def shot(atShot: Coord) = Ships(ships.map(ship => ship.shot(atShot)))
  def allDecks() = ships.map(ship => (ship.cells.zip(ship.decks))).flatten
  def isAllBroken() = ships.forall(_.decks.forall(_ == Deck(isBroken = true)))
}

object Ships {
  def default() = Ships(Array())
}

final case class Field(field: Vector[Vector[Cell]]) {
  def shot(shotAt: Coord) =
    Field(field.updated(shotAt.y, field(shotAt.y).updated(shotAt.x, Harmed())))

  def cellAt(coord: Coord) = field(coord.y)(coord.x)
}

object Field {
  def default() =
    Field(Vector.fill(Consts.fieldSideSize, Consts.fieldSideSize)(Unharmed()))
}

final case class State(ships: Ships, field: Field = Field.default()) {
  def shot(shotAt: Coord): State =
    State(field = field.shot(shotAt), ships = ships.shot(shotAt))

  private def render(
      renderF: (Option[(Coord, Deck)], Int, Int) => Char
  ): Array[String] = {
    val shipsDecks = ships.allDecks()
    val chars = for {
      y <- 0 until Consts.fieldSideSize
      x <- 0 until Consts.fieldSideSize
    } yield renderF((shipsDecks.find(deck => deck._1 == Coord(x, y))), x, y)
    chars
      .grouped(10)
      .foldLeft(Array[String]())(
        (acc, line) =>
          acc :+ line.foldLeft("")((acc, char) => acc :+ char :+ ' ')
      )
  }
  def renderForMe(): Array[String] = {
    render {
      case (mbDeck, x, y) =>
        mbDeck match {
          case Some((_, deck)) => deck.render
          case None            => field.cellAt(Coord(x, y)).render
        }
    }
  }
  def renderForEnemy(): Array[String] = {
    render {
      case (mbDeck, x, y) =>
        mbDeck match {
          case Some((_, deck)) if field.cellAt(Coord(x, y)) == Harmed() =>
            deck.render
          case None => field.cellAt(Coord(x, y)).render
          case _    => Unharmed().render()
        }
    }
  }
}

final case class Player(
    private val tx: ObjectOutputStream,
    private val rx: ObjectInputStream,
    private val state: State
) {
  def shot(shotAt: Coord): Player = copy(state = state.shot(shotAt))
  def makeShot() = {
    rx.readObject().asInstanceOf[Coord]
  }
  def isAlive() = !state.ships.isAllBroken

  def renderForEnemy = state.renderForEnemy
  def renderForMe = state.renderForMe

  def send[A <: Serializable](data: A) = {
    tx.writeObject(data)
    tx.flush
  }
}
