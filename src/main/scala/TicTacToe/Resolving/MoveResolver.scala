package TicTacToe.Resolving

import TicTacToe.Models.{Move, Player}

trait MoveResolver
  extends MiniMax
{

  def resolve(moves: List[Move], player: Player.Value, deep: Int) ={
    val board = fillBoard(moves, emptyBoard())
    val posMoves = board.filter(m => m.player.isEmpty)
    val move = miniMax(board, posMoves, player, deep)
    if(move.isDefined){
      apply(move.get, Some(player))
    }
    move
  }
}
