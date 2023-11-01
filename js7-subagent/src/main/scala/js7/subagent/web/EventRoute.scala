package js7.subagent.web

import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.subagent.{SubagentRunId, SubagentState}
import js7.journal.web.GenericEventRoute
import js7.subagent.SubagentCommandExecutor
import org.apache.pekko.http.scaladsl.server.Directives.*

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
