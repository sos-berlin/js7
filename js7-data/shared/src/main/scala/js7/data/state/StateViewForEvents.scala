package js7.data.state

import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}

object StateViewForEvents {
  implicit final class StateViewForEvents(private val stateView: StateView) extends AnyVal {
    import stateView.*

    def atController(events: => List[OrderActorEvent]): List[OrderActorEvent] =
      if (isAgent)
        OrderDetachable :: Nil
      else
        events
  }
}
