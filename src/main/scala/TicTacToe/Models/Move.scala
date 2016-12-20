package TicTacToe.Models

object Player extends Enumeration {
  val X, O = Value
}

case class Move(x: Int, y: Int, player: Option[Player.Value], rank: Option[Int]){
  def index = x * 3 + y

  def asJson: String = {
    "{\"x\":"+ x + ",\"y\":" + y + ",\"v\":\"" + player.get + "\"}"
  }
}

