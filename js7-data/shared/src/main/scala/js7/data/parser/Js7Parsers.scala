package js7.data.parser

import cats.parse.Parser
import js7.data.agent.AgentPath
import js7.data.item.VersionedItemPath
import js7.data.lock.LockPath
import js7.data.parser.CatsBasicParsers.{checkedToParser, pathString, quotedString}

object Js7Parsers
{
  def path[A <: VersionedItemPath](A: VersionedItemPath.Companion[A]): Parser[A] =
    pathString.flatMap(p => checkedToParser(A.checked(p)))

  val agentPath: Parser[AgentPath] =
    pathString
      .flatMap(string => checkedToParser(AgentPath.checked(string)))

  val quotedLockPath: Parser[LockPath] =
    quotedString
      .flatMap(string => checkedToParser(LockPath.checked(string)))
}
