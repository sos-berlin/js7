package js7.data.execution.workflow.instructions

import java.time.ZoneId
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTime.extensions.toZoneId
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.execution.workflow.instructions.AdmissionTimeExecutor.*
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderOutcomeAdded, OrderWaitingForAdmission}
import js7.data.order.{Order, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.AdmissionTime
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private[instructions] final class AdmissionTimeExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = AdmissionTime
  val instructionClass = classOf[AdmissionTime]

  def toEvents(instr: AdmissionTime, order: Order[Order.State], state: StateView) =
    if !order.isState[IsFreshOrReady] && !order.isState[Order.WaitingForAdmission] then
      Right(Nil)
    else
      detach(order).getOrElse:
        start(order).getOrElse:
          for
            workflow <- state.idToWorkflow.checked(order.workflowId)
            given ZoneId <- workflow.timeZone.toZoneId
          yield
            val now = clock.now()
            instr.admissionTimeScheme
              .findTimeInterval(now, limit = FindTimeIntervalLimit, dateOffset = noDateOffset)
            match
              case None =>
                List(
                  order.id <-:
                    OrderOutcomeAdded(OrderOutcome.Disrupted(Problem("No more admission time"))),
                  order.id <-: OrderFailedIntermediate_())

              case Some(interval) =>
                if interval.contains(now) then
                  (order.id <-: OrderMoved(order.position / BranchId.AdmissionTime % 0)) :: Nil
                else
                  val start = interval.start
                  order.state match
                    case state @ Order.WaitingForAdmission(until) if until == start =>
                      logger.trace(s"🪱 Useless execution of toEvents: ${order.id} $state")
                      Nil
                    case _ =>
                      (order.id <-: OrderWaitingForAdmission(start)) :: Nil

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
