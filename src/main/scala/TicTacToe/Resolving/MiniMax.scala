package TicTacToe.Resolving

import TicTacToe.Models.{BoardOperations, Move, MoveOperations, Player}

trait MiniMax extends BoardOperations with MoveOperations{
  def miniMax(board: List[Move], ap: List[Move], player: Player.Value, deep: Int):Option[Move] = ap match {
    case List() => None
    case _      =>
      val ranked = ap.par.map(m => apply(m,getRanked(board,MiniMax.Max,player,deep, apply(m, Some(player))))).toList
      possOrZero(ranked, None)
  }

  def getRanked(board: List[Move], miniMax: MiniMax.Value, player: Player.Value, deep: Int, move: Move): Int = deep match {
    case 0 => 0
    case _ =>
      val myMove = apply(move, Some(player))
      val fiilledBoard = fillBoard(List(myMove), board)
      getWinner(fiilledBoard) match {
        case Some(_) => if(miniMax == MiniMax.Max) 10 else -10
        case _       =>
          val a = fiilledBoard.filter(f => f.player.isEmpty)
          a match {
            case List() => 0
            case allPos =>
              val reversedmm = reverse(miniMax)
              val revP = reverse(Some(player)).get
              val ranked = allPos.map(m => getRanked(fiilledBoard,reversedmm,revP,deep -1, m))
              reversedmm match {
                case MiniMax.Min => ranked.min
                case MiniMax.Max => ranked.max
              }
          }
      }
  }

  def possOrZero(moves: List[Move], zero: Option[Move]):Option[Move] = moves match {
    case m :: List() => if(zero.isEmpty) Some(m) else zero
    case m :: tail   =>
      if(m.rank.nonEmpty && m.rank.get > 0) Some(m)
      else if(m.rank.nonEmpty && m.rank.get == 0) possOrZero(tail,Some(m))
      else possOrZero(tail,zero)
  }

  object MiniMax extends Enumeration {
    val Min, Max = Value
  }

  def reverse(miniMax: MiniMax.Value) = miniMax match {
    case MiniMax.Max => MiniMax.Min
    case MiniMax.Min => MiniMax.Max
  }
}
