package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, EventCalc, EventColl}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}

type EngineEventCalc[S <: EngineState_[S]] =
  EngineEventCalc_[S, Event]

type EngineEventCalc_[S <: EngineState_[S], E <: Event] =
  EventCalc[S, E]

type EngineEventColl_[S <: EngineState_[S], E <: Event] =
  EventColl[S, E]

object EngineEventColl:

  object extensions:
    extension [S <: EngineState_[S], E <: Event](coll: EventColl[S, E])
      def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
        coll.aggregate.idToOrder

      def order(orderId: OrderId): Checked[Order[Order.State]] =
        coll.aggregate.idToOrder.checked(orderId)

      //def orderIf[A <: Order.State: ClassTag](orderId: OrderId): Option[Checked[Order[A]]] =
      //  order(orderId).traverse:
      //    _.ifState[A]

      def useOrder[R](orderId: OrderId)(body: Order[Order.State] => Checked[R]): Checked[R] =
        order(orderId).flatMap:
          body

      def workflow(workflowId: WorkflowId): Checked[Workflow] =
        coll.aggregate.idToWorkflow.checked(workflowId)

      def orderAndWorkflow(orderId: OrderId): Checked[(Order[Order.State], Workflow)] =
        useOrder(orderId): order =>
          workflow(order.workflowId).map:
            order -> _

      //def atController[E1 <: Event](orderId: OrderId)(other: => EventCalc[S, E1])
      //: Checked[EventColl[S, OrderDetachable | E1]] =
      //  if coll.aggregate.isAgent then
      //    coll.widen[S, OrderDetachable | E1].addEvent:
      //      orderId <-: OrderDetachable
      //  else
      //    coll.widen[S, OrderDetachable | E1].addEventCalc:
      //      other
