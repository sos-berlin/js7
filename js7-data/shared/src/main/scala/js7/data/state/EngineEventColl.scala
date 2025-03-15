package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, EventColl}
import js7.data.order.{Order, OrderId}

type EngineEventColl[S <: EventDrivenStateView[S], E <: Event, Ctx] = EventColl[S, E, Ctx]


object EngineEventColl:

  object extensions:
    extension [S <: EventDrivenStateView[S], E <: Event, Ctx](coll: EngineEventColl[S, E, Ctx])
      def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
        coll.aggregate.idToOrder

      def useOrder[R](orderId: OrderId)(body: Order[Order.State] => Checked[R]): Checked[R] =
        coll.aggregate.idToOrder.checked(orderId).flatMap: order =>
          body(order)
