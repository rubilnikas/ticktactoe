package TicTacToe

import Models.{Move, Player}
import TicTacToe.Http.HttpClient
import TicTacToe.Resolving.MoveResolver

object Main
  extends HttpClient
    with MoveResolver {
  def main(args: Array[String]): Unit = args.toList match {
    case player :: "1" :: id :: tails => atack(toPlayer(player).get, "1", id, List())
    case player :: "2" :: id :: tails => wait(toPlayer(player).get, "2", id)
    case _ => println(Console.RED + " Bad arguments X/O 1/2 id")
  }


  def atack(player: Player.Value, number: String, id: String, moves: List[Move]): Unit = {
    val move = resolve(moves, player, 9)
    val allMoves: List[Move] = moves :+ move.get
    postMoves(id, number, allMoves)

    if (allMoves.length < 8 || getWinner(fillBoard(allMoves, emptyBoard())).isEmpty) {
      wait(player, number, id)
    }
  }

  def wait(player: Player.Value, number: String, id: String): Unit = {
    val moves = getMoves(id, number)

    atack(player, number, id, moves);
  }
}
