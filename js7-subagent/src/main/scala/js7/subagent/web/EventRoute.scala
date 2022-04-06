package js7.subagent.web

import akka.http.scaladsl.server.Directives._
import cats.syntax.traverse._
import js7.agent.data.Problems.{SubagentNotDedicatedProblem, SubagentRunIdMismatchProblem}
import js7.common.akkahttp.StandardMarshallers._
import js7.data.subagent.SubagentRunId
import js7.journal.web.GenericEventRoute
import js7.subagent.{SubagentCommandExecutor, SubagentState}

trait EventRoute extends SubagentRouteProvider with GenericEventRoute
{
  protected val commandExecutor: SubagentCommandExecutor

  protected final lazy val eventRoute = {
    val route = new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = SubagentState.keyedEventJsonCodec
    }.route

    parameter("subagentRunId".?) { subagentRunIdString =>
      subagentRunIdString.traverse(SubagentRunId.checked) match {
        case Left(problem) =>
          complete(problem)

        case Right(maybeSubagentRunId) =>
          commandExecutor.dedicated match {
            case None =>
              complete(SubagentNotDedicatedProblem)

            case Some(dedicated) =>
              if (maybeSubagentRunId.exists(_ != commandExecutor.subagentRunId))
                complete(SubagentRunIdMismatchProblem(dedicated.subagentId))
              else
                route
          }
      }
    }
  }
}
