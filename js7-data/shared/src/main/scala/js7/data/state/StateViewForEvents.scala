package js7.data.state

import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}

object StateViewForEvents:

  extension (state: StateView)

    def atController(events: => List[OrderActorEvent]): List[OrderActorEvent] =
      if state.isAgent then
        OrderDetachable :: Nil
      else
        events
