package sea_battle.player

import java.io.{ObjectOutputStream, ObjectInputStream}

import sea_battle.coord.Coord
import sea_battle.consts.Consts
import sea_battle.ship.{Ships, Deck}
import sea_battle.field.Field
import sea_battle.cell.Cell.{Unharmed, Harmed}
import sea_battle.coord.Coord

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
