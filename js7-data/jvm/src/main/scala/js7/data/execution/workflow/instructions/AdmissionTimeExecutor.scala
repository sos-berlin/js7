package js7.data.execution.workflow.instructions

import java.time.ZoneId
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.extensions.toZoneId
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.{fail, moveOrderDownToBranch, moveOrderToNextInstruction}
import js7.data.execution.workflow.instructions.AdmissionTimeExecutor.*
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderMoved, OrderWaitingForAdmission}
import js7.data.order.{Order, OrderEvent, OrderId, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.EngineEventColl.extensions.workflow
import js7.data.state.EngineState_
import js7.data.workflow.instructions.AdmissionTime
import js7.data.workflow.position.*

private object AdmissionTimeExecutor extends EventInstructionExecutor_[AdmissionTime]:
  private val logger = Logger[this.type]
  private val noDateOffset = ExecuteExecutor.noDateOffset
  private val FindTimeIntervalLimit = ExecuteExecutor.FindTimeIntervalLimit

  def toEventCalc[S <: EngineState_[S]](instr: AdmissionTime, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      for
        workflow <- coll.workflow(order.workflowId)
        given ZoneId <- workflow.timeZone.toZoneId
        coll <-
          if !order.isState[IsFreshOrReady] && !order.isState[Order.WaitingForAdmission] then
            coll.nix
          else if order.forceAdmission then
            coll:
              moveOrderDownToBranch(order, BranchId.AdmissionTime)
          else skippedReason(order, instr) match
            case Some(reason) =>
              /// Skip ///
              // No need for OrderStarted
              coll:
                moveOrderToNextInstruction(order, Some(reason))
            case None =>
              // OPTIMISE: Result of findTimeInterval could be cached for the workflow position,
              // to avoid re-evaluation for each order.
              val now = coll.now
              instr.admissionTimeScheme
                .findTimeInterval(now, limit = FindTimeIntervalLimit, dateOffset = noDateOffset)
              match
                case None =>
                  /// No more admission time ///
                  // Fail even if Order hasn't started
                  coll:
                    fail[S](orderId, Some(OrderOutcome.Disrupted(Problem("No more admission time"))))

                case Some(interval) =>
                  if !order.isStarted && order.isDelayed(now) then
                    /// Wait for Fresh Order's scheduled time ///
                    coll.nix
                  else if interval.contains(now) then
                    /// Enter the AdmissionTime block ///
                    coll:
                      moveOrderDownToBranch(order, BranchId.AdmissionTime)
                  else
                    /// Wait for admission ///
                    // When detaching, the Controller checks the admission time with its own clock
                    detach(coll, order): coll =>
                      start(coll, orderId): (coll, order) =>
                        val start = interval.start
                        order.state match
                          case state @ Order.WaitingForAdmission(until) if until == start =>
                            logger.trace(s"🪱 Useless call: ${order.id} $state")
                            coll.nix
                          case _ =>
                            coll:
                              order.id <-: OrderWaitingForAdmission(start)
      yield coll

  private def skippedReason(order: Order[Order.State], instr: AdmissionTime)(using ZoneId)
  : Option[OrderMoved.Reason] =
    ExecuteExecutor.skippedBecauseOrderDayHasNoAdmissionPeriodStart(
      order, Some(instr.admissionTimeScheme),
      skipIfNoAdmissionStartForOrderDay = instr.skipIfNoAdmissionStartForOrderDay
    ) ? OrderMoved.NoAdmissionPeriodStart

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    order.state match
      case Order.WaitingForAdmission(until) =>
        Right(Set(OrderObstacle.WaitingForAdmission(until)))

      case _ =>
        super.toObstacles(order, calculator, now)
