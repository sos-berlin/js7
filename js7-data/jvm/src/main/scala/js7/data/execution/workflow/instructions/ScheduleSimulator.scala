package js7.data.execution.workflow.instructions

import js7.base.time.ScalaTime._
import js7.base.time.{TestWallClock, Timestamp}
import js7.data.execution.workflow.instructions.ScheduleSimulator._
import js7.data.order.CycleState
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.FiniteDuration

trait ScheduleSimulator {
  this: ScheduleCalculator =>

  def simulate(start: Timestamp, limit: Int, jobExecutionTime: FiniteDuration = 0.s): Result = {
    val clock = TestWallClock(start)
    val initialCycleState = CycleState.initial(until = nextMidnight(clock.now()))
    val builder = new VectorBuilder[Scheduled]

    @tailrec def loop(cycleState: CycleState, i: Int): Unit =
      nextCycleState(cycleState, clock.now()) match {
        case Some(nextCycleState) if i <= limit =>
          builder += Scheduled(clock.now(), nextCycleState)
          if (nextCycleState.next >= clock.now()) {
            clock := nextCycleState.next
          }
          clock += jobExecutionTime
          loop(nextCycleState, i + 1)

        case _ =>
      }

    loop(initialCycleState, 1)
    Result(builder.result(), clock.now())
  }
}

object ScheduleSimulator
{
  final case class Result(scheduledSeq: Seq[Scheduled], exitAt: Timestamp)

  final case class Scheduled(arriveAt: Timestamp, cycleState: CycleState)
  {
    def next = cycleState.next
  }
}