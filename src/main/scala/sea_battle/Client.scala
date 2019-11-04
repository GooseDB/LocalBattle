package sea_battle.client

import java.net.Socket
import java.net.InetSocketAddress
import java.io.{ObjectOutputStream, ObjectInputStream}
import scala.annotation.tailrec

import sea_battle.common.ShipCreationData
import sea_battle.player.State
import sea_battle.coord.Coord
import sea_battle.consts.Consts
import sea_battle.game_over.GameOver
import sea_battle.ship.{Ships, Ship}

object Main {
  def main(args: Array[String]): Unit = {
    val message = args.headOption
      .map(_.toIntOption)
      .flatten
      .map(serverPort => runClient(serverPort)) match {
      case Some(Right(())) => "Bye :)"
      case Some(Left(err)) => s"Sorry, $err"
      case None            => "Port for server is required and must be integer :("
    }
    println(message)
  }

  def runClient(serverPort: Int): Either[String, Unit] = {
    val mySocket = new Socket()
    mySocket.connect(new InetSocketAddress("localhost", serverPort))

    val tx = new ObjectOutputStream(mySocket.getOutputStream())
    var rx = new ObjectInputStream(mySocket.getInputStream())

    val ships = createShips()
    println("Ships have been created")

    tx.writeObject(ships)
    println("Ships have been sent")

    val amIFirst = rx.readBoolean()
    loop(tx, rx, amIFirst)

    mySocket.close
    Right(())
  }

  @tailrec
  def loop(
      tx: ObjectOutputStream,
      rx: ObjectInputStream,
      myTurn: Boolean
  ): Unit = {
    val (gameOver, fields) = if (myTurn) {
      print("Make shot:")
      val shot = makeShot()

      tx.writeObject(shot)

      val fields = rx.readObject().asInstanceOf[Array[(String, String)]]
      val gameOver = rx.readObject().asInstanceOf[GameOver]

      (gameOver, fields)
    } else {
      print("Wait..")

      val fields = rx.readObject().asInstanceOf[Array[(String, String)]]
      val gameOver = rx.readObject().asInstanceOf[GameOver]

      (gameOver, fields)
    }
    drawTwoFields(fields)
    if (!gameOver) loop(tx, rx, !myTurn) else println("Game over!")
  }

  def createShips(): Ships = {

    val shipsSizes = for {
      shipSize <- 1 to 4
      count <- 1 to (5 - shipSize)
    } yield shipSize

    println(Consts.inputFormatTip)

    drawOneField(State(Ships(Array[Ship]())).renderForMe())

    val ships = shipsSizes.foldLeft(Array[Ship]())((ships, shipSize) => {
      println(Consts.separator)
      println(s"Create $shipSize deck ship.")
      val newShip = ShipCreationData.createShip(ships, shipSize)
      val newShips = ships :+ newShip
      drawOneField(State(Ships(newShips)).renderForMe())
      newShips
    })
    Ships(ships)
  }

  def drawOneField(field: Array[String]) = {
    println(Consts.scale)
    field.zipWithIndex.foreach {
      case (line, index) => printf("%2d %s\n", index + 1, line)
    }
  }

  def drawTwoFields(fields: Array[(String, String)]) = {
    println(Consts.scale ++ " " ++ Consts.scale)
    fields.zipWithIndex.foreach {
      case ((my, enemy), index) =>
        printf("%2d %s %2d %s\n", index + 1, my, index + 1, enemy)
    }
  }

  @scala.annotation.tailrec
  def makeShot(): Coord = {
    Coord.validate(scala.io.StdIn.readLine()) match {
      case Left(err) => {
        println(s"$err. Try again")
        makeShot()
      }
      case Right(value) => value
    }

  }
}
