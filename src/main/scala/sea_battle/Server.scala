package sea_battle.server

import java.net.ServerSocket
import java.net.SocketException
import java.io.{ObjectOutputStream, ObjectInputStream}
import scala.annotation.tailrec

import sea_battle.player.{Player, State}
import sea_battle.game_over.GameOver
import sea_battle.ship.Ships

final case class Game(active: Player, waiting: Player) {
  @tailrec
  def run(): Game = {
    val shot = active.makeShot()
    val shotWaiting = waiting.shot(shot)
    if (shotWaiting.isAlive()) Game(shotWaiting, active).run() else this
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val message = args.headOption
      .map(_.toIntOption)
      .flatten
      .map(port => runServer(port)) match {
      case Some(Right(())) => "Bye :)"
      case Some(Left(err)) => s"Sorry, $err"
      case None            => "Port for server is required and must be integer :("
    }
    println(message)
  }

  def runServer(port: Int): Either[String, Unit] = {

    val server = try {
      new ServerSocket(port)
    } catch {
      case e: IllegalArgumentException => return Left("Illegal port")
      case e: SocketException =>
        return Left(s"Unable to create socket: ${e.getMessage()}")
    }

    println("Waiting for players..")

    val first = server.accept()
    val firstTx = new ObjectOutputStream(first.getOutputStream())
    firstTx.flush()
    val firstRx = new ObjectInputStream(first.getInputStream())
    println("First player has been conneted")

    val second = server.accept()
    val secondTx = new ObjectOutputStream(second.getOutputStream())
    secondTx.flush()
    val secondRx = new ObjectInputStream(second.getInputStream())
    println("Second player has been connected")

    server.close

    println("Waiting for ships..")

    val firstShips = firstRx.readObject().asInstanceOf[Ships]
    println("First ships has been recieved")

    val secondShips = secondRx.readObject().asInstanceOf[Ships]
    println("Second ships has been recieved")

    firstTx.writeBoolean(true)
    firstTx.flush()

    secondTx.writeBoolean(false)
    secondTx.flush()

    println("Order is sent.")

    val firstPlayer = Player(firstTx, firstRx, State(firstShips))
    val secondPlayer = Player(secondTx, secondRx, State(secondShips))
    Right(loop(Game(firstPlayer, secondPlayer)))
  }

  def loop(game: Game): Unit = {
    val shot = game.active.makeShot()
    println(s"Got shot: $shot")

    val shotWaiting = game.waiting.shot(shot)

    val forActive = game.active.renderForMe.zip(shotWaiting.renderForEnemy)
    val forWaiting = shotWaiting.renderForMe.zip(game.active.renderForEnemy)

    game.active.send(forActive)
    game.waiting.send(forWaiting)

    val gameOver: GameOver = !shotWaiting.isAlive()
    game.active.send(gameOver)
    game.waiting.send(gameOver)

    if (gameOver) println("Game over")
    else loop(Game(shotWaiting, game.active))
  }
}
