package js7.data.state

import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable}

object StateViewForEvents:

  extension (state: StateView)

    def atController[E <: OrderActorEvent](events: => List[E]): List[E | OrderDetachable] =
      if state.isAgent then
        OrderDetachable :: Nil
      else
        events
