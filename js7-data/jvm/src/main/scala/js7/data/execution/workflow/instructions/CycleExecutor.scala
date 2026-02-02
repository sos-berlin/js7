package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.extensions.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.event.{EventCalc, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.execution.workflow.instructions.ScheduleCalculator.Do
import js7.data.order.Order.{BetweenCycles, Ready}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderMoved}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{CycleState, Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Cycle

private object CycleExecutor extends EventInstructionExecutor_[Cycle]:

  def toEventCalc[S <: EngineState_[S]](instr: Cycle, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      start(coll, orderId): (coll, order) =>
        order.ifState[Ready].map: order =>
          coll:
            readyEvents(instr, order)
        .orElse:
          order.ifState[BetweenCycles].map: order =>
            coll:
              betweenCyclesEvents(instr, order)
        .getOrElse:
          coll.nix

  private def readyEvents[S <: EngineState_[S]](instr: Cycle, order: Order[Order.Ready])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      for
        workflow <- coll.aggregate.keyToItem(Workflow).checked(order.workflowId)
        (calendar, calculator) <- toCalendarAndScheduleCalculator(workflow, instr, coll.aggregate)
        calendarExecutor <- CalendarExecutor.checked(calendar, workflow.timeZone)
        timeInterval <- calendarExecutor.orderIdToTimeInterval(order.id)
        coll <-
          calculator.nextCycleState(
            CycleState(
              next = timeInterval.start,
              end = timeInterval.end,
              schemeIndex = -1,
              periodIndex = -1,
              index = 0),
            coll.now)
          match
            case Some(cycleState) =>
              coll:
                order.id <-: OrderCyclingPrepared(cycleState)
            case None =>
              coll:
                endCycling(order)
      yield coll

  private def betweenCyclesEvents[S <: EngineState_[S]]
    (instr: Cycle, order: Order[Order.BetweenCycles])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      order.state.cycleState match
        case None =>
          coll:
            endCycling(order)

        case Some(cycleState) =>
          toScheduleCalculator(order, instr, coll.aggregate).flatMap:
            _.onNextCycleIsDue(cycleState, coll.now)
          .flatMap:
            case Do.KeepWaiting =>
              coll.nix

            case Do.StartCycle(skipped) =>
              coll:
                order.id <-: OrderCycleStarted(skipped)

            case Do.ChangeCycleState(cycleState) =>
              coll:
                order.id <-: OrderCyclingPrepared(cycleState)

            case Do.EndCycling =>
              coll:
                endCycling(order)

  private def endCycling[S <: EngineState_[S]](order: Order[Ready | BetweenCycles]): EventCalc[S, OrderMoved] =
    moveOrderToNextInstruction(order)

  override def onReturnFromSubworkflow[S <: EngineState_[S]](instr: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      for
        calculator <- toScheduleCalculator(order, instr, coll.aggregate)
        branchId <- order.position.branchPath.lastOption.map(_.branchId)
          .toChecked(Problem(s"${order.id} Cycle Position expected: ${order.position}"))
        cycleState <- branchId.toCycleState
        coll <- coll:
          order.id <-: OrderCycleFinished(calculator.nextCycleState(cycleState, coll.now))
      yield coll

  private def toScheduleCalculator(order: Order[Order.State], cycle: Cycle, state: EngineState) =
    for
      workflow <- state.keyToItem(Workflow).checked(order.workflowId)
      (_, calculator) <- toCalendarAndScheduleCalculator(workflow, cycle, state)
    yield
      calculator

  private def toCalendarAndScheduleCalculator(workflow: Workflow, cycle: Cycle, state: EngineState)
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
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    order.state match
      case BetweenCycles(Some(cycleState: CycleState)) if now < cycleState.next =>
        Right(Set(WaitingForOtherTime(cycleState.next)))

      case _ => super.toObstacles(order, calculator, now)
