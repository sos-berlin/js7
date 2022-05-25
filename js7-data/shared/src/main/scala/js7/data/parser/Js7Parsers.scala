package js7.data.parser

import cats.parse.Parser
import js7.data.item.InventoryItemPath
import js7.data.parser.CatsBasicParsers.{checkedToParser, quotedString}

object Js7Parsers
{
  private val pathString: Parser[String] =
    quotedString

  def path[A <: InventoryItemPath](A: InventoryItemPath.Companion[A]): Parser[A] =
    pathString.flatMap(p => checkedToParser(A.checked(p)))
}
