package TicTacToe.Http

import TicTacToe.Json.Deserialization
import TicTacToe.Models.Move
import com.sun.prism.paint.Color
import play.api.libs.ws.ning.NingWSClient

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class HttpClient extends Deserialization {


  def getMoves(id: String, player: String): List[Move] = {
    val wsClient = NingWSClient()
    try {
      val task = wsClient
        .url(getUrl(id, player))
        .withHeaders("Accept" -> "application/json+map")
        .get()
        .map { wsResponse =>
          if (!(200 to 299).contains(wsResponse.status)) {
            sys.error(s"Received unexpected status ${wsResponse.status} : ${wsResponse.body}")
          }

          parseAll(value, wsResponse.body).get.asInstanceOf[List[Move]]

        }

      Await.result(task, 10 seconds)
    }
    finally {
      wsClient.close()
    }
  }

  def postMoves(id: String, player: String, moves: List[Move]): Unit = {
    val wsClient = NingWSClient()
    try {
      val numbers = (0 to 8)

      val zipped = moves.zip(numbers)

      val json = "{" + zipped.map(z =>  "\"" + z._2 + "\":" + z._1.asJson ).mkString(",") + "}"

      val task = wsClient
        .url(getUrl(id, player))
        .withHeaders("Content-Type" -> "application/json+map")
        .post(json)
        .map { wsResponse =>
          if (!(200 to 299).contains(wsResponse.status)) {
            sys.error(s"Received unexpected status ${wsResponse.status} : ${wsResponse.body}")
          }
        }

      Await.result(task, 1 seconds)
    } finally {
      wsClient.close()
    }
  }

  private def getUrl(id: String, player: String) = s"http://tictactoe.homedir.eu/game/$id/player/$player"
}
