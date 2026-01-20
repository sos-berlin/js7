package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, EventColl}
import js7.data.order.{Order, OrderId}

object EngineEventColl:

  object extensions:
    extension [S <: EngineState_[S], E <: Event, Ctx](coll: EventColl[S, E, Ctx])
      def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
        coll.aggregate.idToOrder

      def useOrder[R](orderId: OrderId)(body: Order[Order.State] => Checked[R]): Checked[R] =
        coll.aggregate.idToOrder.checked(orderId).flatMap: order =>
          body(order)
