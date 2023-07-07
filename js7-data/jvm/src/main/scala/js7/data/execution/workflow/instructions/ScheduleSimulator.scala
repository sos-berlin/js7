package js7.data.execution.workflow.instructions

import js7.base.time.ScalaTime.*
import js7.base.time.{TimeInterval, Timestamp}
import js7.data.execution.workflow.instructions.ScheduleSimulator.*
import js7.data.order.CycleState
import scala.collection.{AbstractIterator, View}
import scala.concurrent.duration.FiniteDuration

trait ScheduleSimulator {
  this: ScheduleCalculator =>

  def simulate(timeInterval: TimeInterval, actionDuration: FiniteDuration = 0.s): View[Scheduled] =
    View.fromIteratorProvider(() =>
      new AbstractIterator[Scheduled] {
        var timestamp = timeInterval.start

        private var _next: Option[Scheduled] = Some(Scheduled(
          timestamp,
          CycleState.initial(timeInterval)))

        calculateNext()

        def hasNext =
          _next.isDefined

        def next() =
          _next match {
            case None => throw new NoSuchElementException
            case Some(scheduled) =>
              calculateNext()
              scheduled
          }

        def calculateNext() =
          for (scheduled <- _next) yield {
            val maybeScheduled = nextCycleState(timestamp, scheduled.cycleState)
              .map(Scheduled(timestamp, _))
            _next = maybeScheduled
            for (scheduled <- maybeScheduled) {
              if (timestamp < scheduled.next) {
                timestamp = scheduled.next
              }
              timestamp += actionDuration
            }
          }
      })
}

object ScheduleSimulator
{
  final case class Result(scheduledView: View[Scheduled]/*, exitAt: Timestamp*/)

  final case class Scheduled(arrival: Timestamp, cycleState: CycleState)
  {
    def next = cycleState.next
  }
}
