package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.event.KeyedEvent
import js7.data.order.Order.{BetweenCycles, Ready}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderMoved}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{CycleState, Order, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.instructions.Cycle

private[instructions] final class CycleExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = Cycle
  val instructionClass = classOf[Cycle]

  def toEvents(cycle: Cycle, order: Order[Order.State], state: StateView) = {
    val now = clock.now()
    start(order)
      .orElse(order.ifState[Ready].map(order =>
        for {
          pair <- toCalendarAndScheduleCalculator(order, cycle, state)
          (calendar, calculator) = pair
          calendarExecutor <- CalendarExecutor.checked(calendar)
          timeInterval <- calendarExecutor.orderIdToTimeInterval(order.id)
        } yield {
          val cycleState = calculator.nextCycleState(now, CycleState(
                        next = timeInterval.start,
                        end = timeInterval.end,
                        schemeIndex = -1,
                        index = 0))
          nextCycleStateToEvent(cycleState, order)
        }))
      .orElse(order.ifState[BetweenCycles].map(order =>
        order.state.cycleState match {
          case None =>
            Right(
              (order.id <-: OrderMoved(order.position.increment)) :: Nil)

          case Some(cycleState) =>
            toScheduleCalculator(order, cycle, state)
              .flatMap(_.maybeRecalcCycleState(now, cycleState))
              .map {
                case None =>
                  // cycleState is still valid
                  (cycleState.next <= now).thenList(
                    order.id <-: OrderCycleStarted)

                case Some(maybeRecalculatedCycleState) =>
                  nextCycleStateToEvent(maybeRecalculatedCycleState, order)
              }
        }))
      .getOrElse(Right(Nil))
  }

  private def nextCycleStateToEvent(cycleState: Option[CycleState], order: Order[Order.State])
  : List[KeyedEvent[OrderActorEvent]] = {
    val event = cycleState match {
      case Some(cycleState) => OrderCyclingPrepared(cycleState)
      case None => OrderMoved(order.position.increment)
    }
    (order.id <-: event) :: Nil
  }

  def nextPosition(instruction: Cycle, order: Order[Order.State], stateView: StateView) =
    Right(None)

  private[workflow] def onReturnFromSubworkflow(order: Order[Order.State], cycle: Cycle,
    state: StateView)
  : Checked[KeyedEvent[OrderActorEvent]] =
    for {
      calculator <- toScheduleCalculator(order, cycle, state)
      branchId <- order.position.branchPath.lastOption.map(_.branchId)
        .toChecked(Problem(s"${order.id} Cycle Position expected: ${order.position}"))
      cycleState <- branchId.toCycleState
    } yield
      order.id <-: OrderCycleFinished(
        calculator.nextCycleState(clock.now(), cycleState))

  private def toScheduleCalculator(order: Order[Order.State], cycle: Cycle, state: StateView) =
    for (pair <- toCalendarAndScheduleCalculator(order, cycle, state)) yield
      pair._2

  private def toCalendarAndScheduleCalculator(
    order: Order[Order.State],
    cycle: Cycle,
    state: StateView)
  : Checked[(Calendar, ScheduleCalculator)] =
    for {
      workflow <- state.idToWorkflow.checked(order.workflowId)
      calendarPath <- workflow.calendarPath
        .toChecked(Problem("Cycle instruction requires Workflow.calendarPath"))
      calendar <- state.keyToItem(Calendar).checked(calendarPath)
      zone <- calendar.timezone.toZoneId
      calculator <- ScheduleCalculator.checked(cycle.schedule, zone, calendar.dateOffset)
    } yield calendar -> calculator

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator) =
    order.state match {
      case Order.BetweenCycles(Some(cycleState: CycleState)) if clock.now() < cycleState.next =>
        Right(Set(WaitingForOtherTime(cycleState.next)))

      case _ => super.toObstacles(order, calculator)
    }
}
