package TicTacToe.Models

trait BoardOperations {
  def getWinner(moves: List[Move]) = moves map (m => m.player) match {
    case a :: b :: c :: k :: _ :: _ :: _ :: _ :: _ :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ :: _ :: _ :: a :: b :: c :: _ :: _ :: _ :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ :: _ :: _ :: _ :: _ :: _ :: a :: b :: c :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case a :: _ :: _ :: b :: _ :: _ :: c :: _ :: _ :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ :: a :: _ :: _ :: b :: _ :: _ :: c :: _ :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ :: _ :: a :: _ :: _ :: b :: _ :: _ :: c :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case a :: _ :: _ :: _ :: b :: _ :: _ :: _ :: c :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ :: _ :: a :: _ :: b :: _ :: c :: _ :: _ :: _ if a.nonEmpty && a == b && b == c => Some(a)
    case _ =>   None
  }

  def emptyBoard() = for (x <- List.range(0, 3);y <- List.range(0, 3))yield Move(x, y, None, None)

  def fillBoard(moves: List[Move], board: List[Move]): List[Move] = moves match {
    case List() => board
    case m::tail =>
      fillBoard(tail, board.patch(m.index,Seq(m),1))
  }
}
