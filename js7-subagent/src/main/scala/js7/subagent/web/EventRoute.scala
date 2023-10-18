package js7.subagent.web

import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.subagent.{SubagentRunId, SubagentState}
import js7.journal.web.GenericEventRoute
import js7.subagent.Subagent
import org.apache.pekko.http.scaladsl.server.Directives.*

private trait EventRoute extends SubagentRouteProvider with GenericEventRoute:
  protected val subagent: Subagent

  protected final lazy val eventRoute =
    val route = new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = SubagentState.keyedEventJsonCodec
    }.route

    parameter("subagentRunId")(subagentRunIdString =>
      (for
        dedicatedSubagent <- subagent.checkedDedicatedSubagent
        subagentRunId <- SubagentRunId.checked(subagentRunIdString)
        _ <- dedicatedSubagent.checkSubagentRunId(subagentRunId)
      yield ())
        .fold(complete(_), _ => route))
