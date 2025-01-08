package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, EventColl}
import js7.data.order.{Order, OrderId}

type EngineEventColl[S <: EventDrivenStateView[S]] = EventColl[S, Event]

object EngineEventColl:

  def apply[S <: EventDrivenStateView[S]](aggregate: S): EngineEventColl[S] =
    EventColl[S, Event](aggregate)

  object extensions:
    extension [S <: EventDrivenStateView[S]](coll: EngineEventColl[S])
      def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
        coll.aggregate.idToOrder

      def withOrder[R](orderId: OrderId)(body: Order[Order.State] => Checked[R]): Checked[R] =
        coll.aggregate.idToOrder.checked(orderId).flatMap: order =>
          body(order)