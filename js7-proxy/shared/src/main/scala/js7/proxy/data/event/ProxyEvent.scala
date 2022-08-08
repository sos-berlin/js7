package js7.proxy.data.event

import js7.base.problem.Problem
import js7.data.event.EventId

sealed trait ProxyEvent

object ProxyEvent
{
  final case class ProxyCouplingError(problem: Problem)
  extends ProxyEvent

  final case class ProxyCoupled(after: EventId)
  extends ProxyEvent

  case object ProxyDecoupled
  extends ProxyEvent
}
