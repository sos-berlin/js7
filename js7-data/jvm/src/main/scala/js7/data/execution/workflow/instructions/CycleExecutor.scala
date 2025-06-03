package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.ScheduleCalculator.Do
import js7.data.order.Order.{BetweenCycles, Ready}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderMoved}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{CycleState, Order, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Cycle

private[instructions] final class CycleExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Cycle
  val instructionClass = classOf[Cycle]

  def toEvents(instr: Cycle, order: Order[Order.State], state: StateView) =
    start(order)
      .orElse(order.ifState[Ready].map: order =>
        for
          workflow <- state.keyToItem(Workflow).checked(order.workflowId)
          (calendar, calculator) <- toCalendarAndScheduleCalculator(workflow, instr, state)
          calendarExecutor <- CalendarExecutor.checked(calendar, workflow.timeZone)
          timeInterval <- calendarExecutor.orderIdToTimeInterval(order.id)
        yield
          calculator.nextCycleState(
            CycleState(
              next = timeInterval.start,
              end = timeInterval.end,
              schemeIndex = -1,
              periodIndex = -1,
              index = 0),
            clock.now())
          match
            case Some(cycleState) =>
              (order.id <-: OrderCyclingPrepared(cycleState)) :: Nil
            case None =>
              endCycling(order))
      .orElse(order.ifState[BetweenCycles].map: order =>
        order.state.cycleState match
          case None =>
            Right(endCycling(order))

          case Some(cycleState) =>
            toScheduleCalculator(order, instr, state)
              .flatMap:
                _.onNextCycleIsDue(cycleState, clock.now())
              .map:
                case Do.KeepWaiting => Nil

                case Do.StartCycle(skipped) =>
                  (order.id <-: OrderCycleStarted(skipped)) :: Nil

                case Do.ChangeCycleState(cycleState) =>
                  (order.id <-: OrderCyclingPrepared(cycleState)) :: Nil

                case Do.EndCycling =>
                  endCycling(order))
      .getOrElse:
        Right(Nil)

  private def endCycling(order: Order[Ready | BetweenCycles]): List[KeyedEvent[OrderMoved]] =
    (order.id <-: OrderMoved(order.position.increment)) :: Nil

  override def onReturnFromSubworkflow(instr: Cycle, order: Order[Order.State], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    locally:
      for
        calculator <- toScheduleCalculator(order, instr, state)
        branchId <- order.position.branchPath.lastOption.map(_.branchId)
          .toChecked(Problem(s"${order.id} Cycle Position expected: ${order.position}"))
        cycleState <- branchId.toCycleState
      yield
        order.id <-: OrderCycleFinished(
          calculator.nextCycleState(cycleState, clock.now()))
    .map(_ :: Nil)

  private def toScheduleCalculator(order: Order[Order.State], cycle: Cycle, state: StateView) =
    for
      workflow <- state.keyToItem(Workflow).checked(order.workflowId)
      pair <- toCalendarAndScheduleCalculator(workflow, cycle, state)
    yield
      pair._2

  private def toCalendarAndScheduleCalculator(
    workflow: Workflow,
    cycle: Cycle,
    state: StateView)
  : Checked[(Calendar, ScheduleCalculator)] =
    for
      calendarPath <- workflow.calendarPath
        .toChecked(Problem("Cycle instruction requires Workflow.calendarPath"))
      calendar <- state.keyToItem(Calendar).checked(calendarPath)
      zone <- workflow.timeZone.toZoneId
      calculator <- ScheduleCalculator.checked(cycle.schedule, zone, calendar.dateOffset,
        onlyOnePeriod = cycle.onlyOnePeriod)
    yield 
      calendar -> calculator

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator) =
    order.state match
      case BetweenCycles(Some(cycleState: CycleState)) if clock.now() < cycleState.next =>
        Right(Set(WaitingForOtherTime(cycleState.next)))

      case _ => super.toObstacles(order, calculator)
