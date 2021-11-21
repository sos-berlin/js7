package js7.subagent.web

import js7.journal.web.GenericEventRoute
import js7.subagent.SubagentState

trait EventRoute extends SubagentRouteProvider with GenericEventRoute
{
  protected final lazy val eventRoute =
    new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = SubagentState.keyedEventJsonCodec
    }.route
}
