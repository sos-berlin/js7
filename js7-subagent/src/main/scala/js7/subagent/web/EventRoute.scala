package js7.subagent.web

import akka.http.scaladsl.server.Directives._
import js7.common.akkahttp.StandardMarshallers._
import js7.data.subagent.SubagentRunId
import js7.journal.web.GenericEventRoute
import js7.subagent.{SubagentCommandExecutor, SubagentState}

private trait EventRoute extends SubagentRouteProvider with GenericEventRoute
{
  protected val commandExecutor: SubagentCommandExecutor

  protected final lazy val eventRoute = {
    val route = new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = SubagentState.keyedEventJsonCodec
    }.route

    parameter("subagentRunId")(subagentRunIdString =>
      (for {
        subagentRunId <- SubagentRunId.checked(subagentRunIdString)
        _ <- commandExecutor.checkedDedicated // Subagent must be dedicated
        _ <- commandExecutor.checkSubagentRunId(subagentRunId)
      } yield ())
        .fold(complete(_), _ => route))
  }
}
