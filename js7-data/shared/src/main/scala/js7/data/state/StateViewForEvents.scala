package js7.data.state

import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}

object StateViewForEvents:
  extension (stateView: StateView)
    def atController(events: => List[OrderActorEvent]): List[OrderActorEvent] =
      if stateView.isAgent then
        OrderDetachable :: Nil
      else
        events
