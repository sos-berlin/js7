package js7.data.controller

import js7.base.problem.Checked.RichCheckedIterable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderEvent.OrderAddedX
import js7.data.order.{MinimumOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchema, PlanSchemaState}
import js7.data.state.EventDrivenStateView

trait ControllerEventDrivenStateView[Self <: ControllerEventDrivenStateView[Self]]
extends EventDrivenStateView[Self]:
  this: Self =>

  override protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAddedX =>
        val addedOrderId = orderAdded.ownOrderId getOrElse orderId
        for
          _ <- idToOrder.checkNoDuplicate(addedOrderId)
          _ <- checkPlanIsOpen(orderAdded.planId getOrElse PlanId.Global)
          r <- update(addOrders =
            Order.fromOrderAdded(addedOrderId, orderAdded) :: Nil)
        yield
          r

      case _ =>
        super.applyOrderEvent(orderId, event)

  /** @return None for global PlanId.
    */
  final def evalOrderToPlanId(order: MinimumOrder): Checked[Option[PlanId]] =
    val scope = toPlanOrderScope(order)
    keyToItem(PlanSchema).values.toVector.map:
      _.evalOrderToPlanId(scope)
    .combineProblems
    .map(_.flatten)
    .flatMap: planIds =>
      planIds.length match
        case 0 => Right(None)
        case 1 => Right(Some(planIds.head))
        case n => Left(Problem:
          s"${order.id} fits $n Plans: ${planIds.sorted.mkString(", ")
          } — An Order must not fit multiple Plans")

  def checkPlanIsOpen(planId: PlanId): Checked[Unit] =
    for
      planSchemaState <- keyTo(PlanSchemaState).checked(planId.planSchemaId)
      _ <- planSchemaState.checkIsOpen(planId.planKey)
    yield
      ()
