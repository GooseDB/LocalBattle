package sea_battle.game_over

final case class GameOver(gameOver: Boolean) extends java.io.Serializable

object GameOver {
  implicit def gameOverToBoolean(gameOver: GameOver) = gameOver.gameOver
  implicit def BooleantoGameOver(gameOver: Boolean) = GameOver(gameOver)
}
