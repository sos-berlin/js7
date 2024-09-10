package js7.subagent.web

import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.subagent.{SubagentRunId, SubagentState}
import js7.journal.web.GenericEventRoute
import js7.subagent.Subagent
import org.apache.pekko.http.scaladsl.server.Directives.*

private trait EventRoute extends SubagentRouteProvider, GenericEventRoute:

  protected val subagent: Subagent

  protected final lazy val eventRoute =
    val route = genericEventRoute(SubagentState.keyedEventJsonCodec)

    parameter("subagentRunId"): subagentRunIdString =>
      locally:
        for
          dedicatedSubagent <- subagent.checkedDedicatedSubagent
          subagentRunId <- SubagentRunId.checked(subagentRunIdString)
          _ <- dedicatedSubagent.checkSubagentRunId(subagentRunId)
        yield ()
      match
        case Left(problem) => complete(problem)
        case Right(()) => route
