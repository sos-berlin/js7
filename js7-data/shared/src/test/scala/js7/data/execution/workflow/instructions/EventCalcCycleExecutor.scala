package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.calendar.{Calendar, CalendarExecutor, CalendarPath}
import js7.data.event.{EventCalc, TimeCtx}
import js7.data.execution.workflow.instructions.ScheduleCalculator.Do
import js7.data.order.Order.{BetweenCycles, Fresh, Ready}
import js7.data.order.OrderEvent.{OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderMoved, OrderStarted}
import js7.data.order.{CycleState, Order, OrderEvent, OrderId}
import js7.data.state.EventDrivenStateView
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Cycle

private[instructions] final class EventCalcCycleExecutor[S <: EventDrivenStateView[S]]:

  def toEventCalc(orderId: OrderId)
  : EventCalc[S, OrderStarted | OrderCycleStarted | OrderCyclingPrepared | OrderMoved, TimeCtx] =
    EventCalc: coll =>
      for
        coll <- startOrder(orderId).calculate(coll)
        order <- coll.aggregate.idToOrder.checked(orderId)
        instr <- coll.aggregate.instruction_[Cycle](order.workflowPosition)
        coll <-
          order.ifState[Ready].map: order =>
            readyToEvents(instr, order).calculate(coll)
          .orElse:
            order.ifState[BetweenCycles].map: order =>
              betweenCyclesToEvents(instr, order).calculate(coll)
          .getOrElse:
            Right(coll)
      yield
        coll

  private def startOrder(orderId: OrderId): EventCalc[S, OrderStarted, TimeCtx] =
    EventCalc.checked: controllerState =>
      controllerState.idToOrder.checked(orderId).map: order =>
        order.ifState[Fresh].map: order =>
          order.id <-: OrderStarted

  private def readyToEvents(instr: Cycle, order: Order[Ready])
  : EventCalc[S, OrderCyclingPrepared | OrderMoved, TimeCtx] =
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
              schemeIndex = -1, periodIndex = -1, index = 0),
            coll.context.now)
          match
            case Some(cycleState) =>
              coll.add:
                order.id <-: OrderCyclingPrepared(cycleState)
            case None =>
              coll.add:
                endCycling(order)
      yield
        coll

  private def betweenCyclesToEvents(instr: Cycle, order: Order[BetweenCycles])
  : EventCalc[S, OrderCycleStarted | OrderCyclingPrepared | OrderMoved, TimeCtx] =
    EventCalc: coll =>
      order.state.cycleState match
        case None =>
          coll.add(endCycling(order))

        case Some(cycleState) =>
          toScheduleCalculator(order, instr, coll.aggregate)
            .flatMap:
              _.onNextCycleIsDue(cycleState, coll.context.now)
            .flatMap:
              case Do.KeepWaiting =>
                Right(coll)

              case Do.StartCycle(skipped) =>
                coll.add:
                  order.id <-: OrderCycleStarted(skipped)

              case Do.ChangeCycleState(cycleState) =>
                coll.add:
                  order.id <-: OrderCyclingPrepared(cycleState)

              case Do.EndCycling =>
                coll.add:
                  endCycling(order)

  private def endCycling(order: Order[Ready | BetweenCycles]) =
    order.id <-: OrderMoved(order.position.increment)

  def onReturnFromSubworkflow(instr: Cycle, order: Order[Order.State])
  : EventCalc[S, OrderCycleFinished, TimeCtx] =
    EventCalc.checked: controllerState =>
      for
        calculator <- toScheduleCalculator(order, instr, controllerState)
        branchId <- order.position.branchPath.lastOption.map(_.branchId)
          .toChecked(Problem(s"${order.id} Cycle Position expected: ${order.position}"))
        cycleState <- branchId.toCycleState
      yield
        Some:
          order.id <-: OrderCycleFinished:
            calculator.nextCycleState(cycleState, EventCalc.now)

  private def toScheduleCalculator(order: Order[Order.State], cycle: Cycle, state: S) =
    for
      workflow <- state.keyToItem(Workflow).checked(order.workflowId)
      pair <- toCalendarAndScheduleCalculator(workflow, cycle, state)
    yield
      pair._2

  private def toCalendarAndScheduleCalculator(workflow: Workflow, cycle: Cycle, state: S)
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
