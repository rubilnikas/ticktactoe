package TicTacToe.Models

trait MoveOperations {
  def reverse(player: Option[Player.Value]) = player match {
    case Some(Player.X) => Some(Player.O)
    case Some(Player.O) => Some(Player.X)
    case _        => None
  }

  def apply(move: Move, rank: Int) = move match {
    case Move(x,y,p,_) => Move(x,y,p,Some(rank))
  }

  def apply(move: Move, player: Option[Player.Value]) = move match {
    case Move(x,y,_,r) => Move(x,y,player,r)
  }

  def toPlayer(str: String) = str.filterNot(c => c == '"') match {
    case "X" => Some(Player.X)
    case "x" => Some(Player.X)
    case "O" => Some(Player.O)
    case "o" => Some(Player.O)
    case _   => None
  }

}
