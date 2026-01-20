package js7.data.execution.workflow.instructions

import java.time.ZoneId
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.extensions.toZoneId
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.execution.workflow.instructions.AdmissionTimeExecutor.*
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderOutcomeAdded, OrderWaitingForAdmission}
import js7.data.order.{Order, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.EngineState
import js7.data.workflow.instructions.AdmissionTime
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private[instructions] final class AdmissionTimeExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = AdmissionTime
  val instructionClass = classOf[AdmissionTime]

  def toEvents(instr: AdmissionTime, order: Order[Order.State], state: EngineState) =
    if !order.isState[IsFreshOrReady] && !order.isState[Order.WaitingForAdmission] then
      Right(Nil)
    else
      for
        workflow <- state.idToWorkflow.checked(order.workflowId)
        given ZoneId <- workflow.timeZone.toZoneId
        events <-
          if order.forceAdmission then
            // No need for OrderStarted
            Right:
              (order.id <-: OrderMoved(order.position / BranchId.AdmissionTime % 0)) :: Nil
          else
            // skippedReason is independent of the Order's scheduled time
            skippedReason(order, instr).map: reason =>
              /// Skip ///
              // No need for OrderStarted
              Right:
                (order.id <-: OrderMoved(order.position.increment, Some(reason))) :: Nil
            .getOrElse:
              // OPTIMISE: Result of findTimeInterval could be cached for the workflow position,
              //  to avoid re-evaluation for each order.
              val now = clock.now()
              instr.admissionTimeScheme
                .findTimeInterval(now, limit = FindTimeIntervalLimit, dateOffset = noDateOffset)
              match
                case None =>
                  /// No more admission time ///
                  // Fail even if Order hasn't started
                  Right(List(
                    order.id <-:
                      OrderOutcomeAdded(OrderOutcome.Disrupted(Problem("No more admission time"))),
                    order.id <-: OrderFailedIntermediate_()))

                case Some(interval) =>
                  if !order.isStarted && order.isDelayed(now) then
                    /// Wait for Fresh Order's scheduled time ///
                    Right(Nil)
                  else if interval.contains(now) then
                    /// Enter the AdmissionTime block ///
                    Right:
                      (order.id <-: OrderMoved(order.position / BranchId.AdmissionTime % 0)) :: Nil
                  else
                    /// Wait for admission ///
                    // When detaching, the Controller checks the admission time with its own clock
                    detach(order).getOrElse:
                      start(order).getOrElse:
                        Right:
                          val start = interval.start
                          order.state match
                            case state @ Order.WaitingForAdmission(until) if until == start =>
                              logger.trace(s"🪱 Useless execution of toEvents: ${order.id} $state")
                              Nil
                            case _ =>
                              (order.id <-: OrderWaitingForAdmission(start)) :: Nil
      yield
        events

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)

  override def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator) =
    order.state match
      case Order.WaitingForAdmission(until) =>
        Right(Set(OrderObstacle.WaitingForAdmission(until)))

      case _ =>
        super.toObstacles(order, calculator)


object AdmissionTimeExecutor:
  private val logger = Logger[this.type]
  private val noDateOffset = ExecuteExecutor.noDateOffset
  private val FindTimeIntervalLimit = ExecuteExecutor.FindTimeIntervalLimit

  private def skippedReason(order: Order[Order.State], instr: AdmissionTime)(using ZoneId)
  : Option[OrderMoved.Reason] =
    ExecuteExecutor.skippedBecauseOrderDayHasNoAdmissionPeriodStart(
      order, Some(instr.admissionTimeScheme),
      skipIfNoAdmissionStartForOrderDay = instr.skipIfNoAdmissionStartForOrderDay
    ) ? OrderMoved.NoAdmissionPeriodStart
