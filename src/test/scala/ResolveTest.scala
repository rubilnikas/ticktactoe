import TicTacToe.Models.{Move, Player}
import TicTacToe.Resolving.MoveResolver
import org.scalatest.FunSuite

class ResolveTest
  extends FunSuite
  with MoveResolver
{

  test("Can resolve"){
    val move = resolve(List(Move(0,0,Some(Player.X), None)), Player.O, 9)

    assert(move.get.x == 1)
    assert(move.get.y == 1)
  }
}
