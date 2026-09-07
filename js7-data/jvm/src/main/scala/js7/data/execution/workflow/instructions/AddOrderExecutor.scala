package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.time.{Throttle, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.controller.ControllerState
import js7.data.event.{EventCalc, EventColl}
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderOrderAdded, OrderSleeping}
import js7.data.order.{Order, OrderId}
import js7.data.plan.PlanId
import js7.data.state.{EngineState_, UniqueOrderIdScope}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.workflow.instructions.AddOrder

private object AddOrderExecutor extends EventInstructionExecutor_[AddOrder]:

  private val logger = Logger[this.type]

  def toEventCalc[S <: EngineState_[S]](instr: AddOrder, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        if !order.isState[Order.Ready] && !order.isState[Order.Sleeping]
          || order.isDelayed(coll.timestamp)
        then
          coll.nix
        else
          for
            coll <- coll.narrowAggregate[ControllerState]
            coll <- throttle(order, coll) match
              case Left(orderSleeping) =>
                for order <- order.ifState[Order.Sleeping] do
                  logger.debug(s"🪱 One more $orderId <-: $orderSleeping while ${order.state}")
                coll.add(orderId <-: orderSleeping)

              case Right(speedRecord) =>
                for
                  workflowId <- coll.aggregate.workflowPathToId(instr.workflowPath)
                  scope <- coll.aggregate.toImpureOrderExecutingScope(order, coll.timestamp)
                  addedOrderIdString <- instr.orderId.evalAsString(
                    using scope |+| UniqueOrderIdScope(coll.aggregate.idToOrder.keySet))
                  planId <- instr.planId.fold(Checked(order.planId))(PlanId.evalPlanIdExpr(_, scope))
                  coll <- catchProblemAsOrderFailure(coll, orderId):
                    for
                      addedOrderId <- OrderId.checked(addedOrderIdString)
                      _ <- coll.aggregate
                        .checkPlanAcceptsOrders(planId, allowClosedPlan = planId == order.planId)
                      args <- evalExpressionMap(instr.arguments, scope)
                      coll <- coll:
                        // TODO Zeitpunkt und Gewicht des Throttle hinzufügen.
                        //  Dann braucht ControllerState nicht auf Deadline.now zurückzugreifen.
                        order.id <-: OrderOrderAdded(
                          addedOrderId, workflowId, args,
                          planId = planId,
                          innerBlock = instr.innerBlock,
                          startPosition = instr.startPosition,
                          stopPositions = instr.stopPositions,
                          deleteWhenTerminated = instr.deleteWhenTerminated,
                          forceAdmission = instr.forceAdmission,
                          speedRecord = speedRecord)
                      coll <- coll:
                        moveOrderToNextInstruction(order)
                    yield coll
                yield coll
          yield coll

  private def throttle(order: Order[Order.State], coll: EventColl[ControllerState, ?])
  : Either[OrderSleeping, Option[Throttle.Record]] =
    import coll.aggregate.transient.addOrderInstrThrottle
    val speedRecord = Throttle.Record(coll.monotonic, 1)
    addOrderInstrThrottle.tryRecord(speedRecord) match
      case Left(tooFast) =>
        Left:
          // TODO Introduce OrderDelaying event with monotonic time?
          OrderSleeping(
            coll.timestamp + tooFast.delay.roundUpToNext(Timestamp.Epsilon),
            OrderSleeping.Cause.Throttle)

      case Right(_) =>
        // ControllerState will record the speedRecord when applying the OrderOrderAdded event
        Right(addOrderInstrThrottle.isRecorder ? speedRecord)
