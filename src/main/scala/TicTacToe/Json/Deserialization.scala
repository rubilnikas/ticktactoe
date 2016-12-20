package TicTacToe.Json

import TicTacToe.Models.{Move, MoveOperations}
import scala.util.parsing.combinator._

class Deserialization extends JavaTokenParsers with MoveOperations {

  def value: Parser[Any] = moveHolder | empty


  def moveHolder: Parser[List[Move]] = "{" ~ repsep(movel, ",") ~ "}" ^^ { case l ~ moves ~ r => moves }

  def movel: Parser[Move] = stringLiteral ~ ":" ~ move ^^ { case s ~ sep ~ m => m }

  def move: Parser[Move] = "{" ~ number ~ "," ~ number ~ "," ~ player ~ "}" ^^ { case s1 ~ x ~ s2 ~ y ~ s3 ~ p ~ s4 => Move(x, y, toPlayer(p), None) }

  def number: Parser[Int] = stringLiteral ~ ":" ~ wholeNumber ^^ { case s ~ sep ~ number => number.toInt }

  def player: Parser[String] = stringLiteral ~ ":" ~ stringLiteral ^^ { case s ~ s1 ~ p  => p }

  def empty: Parser[List[Move]] = "{}"^^ {s => List()}
}
