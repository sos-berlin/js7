package js7.data.execution.workflow.instructions

import java.time.{LocalTime, ZoneId}
import js7.base.log.Logger
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.problem.Checked._
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime._
import js7.base.time.{AdmissionTimeScheme, AlwaysPeriod, DailyPeriod, TestWallClock, Timestamp, Timezone, WallClock}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.execution.workflow.instructions.CycleExecutorTest._
import js7.data.order.Order.{BetweenCycles, Finished, Ready}
import js7.data.order.OrderEvent.{OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderMoved}
import js7.data.order.{CycleState, Order, OrderEvent, OrderId}
import js7.data.state.StateView
import js7.data.workflow.instructions.Schedule.{Periodic, Scheme, Ticking}
import js7.data.workflow.instructions.{Cycle, CycleTest, ImplicitEnd, Schedule}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration._

final class CycleExecutorTest extends AnyFreeSpec with CycleTester
{
  coupleScribeWithSlf4j()

  private implicit val zone = ZoneId.of("Europe/Mariehamn")

  "Cycle with no Scheme is skipped" in {
    val stepper = new Stepper(
      Workflow(
        workflowId,
        Seq(
          Cycle(
            Schedule(Nil),
            cycleWorkflow = Workflow.empty))),
      WallClock)

    stepper.step()
    assert(stepper.events == Seq(OrderMoved(Position(1))))
    assert(stepper.order == readyOrder.copy(
      state = Ready,
      workflowPosition = workflowId /: Position(1)))
  }

  "Endless empty loop" in {
    val clock = TestWallClock(Timestamp("2021-09-29T21:00:00Z"))
    val stepper = new Stepper(
      Workflow(
        workflowId,
        Seq(
          Cycle(Schedule(
            schemes = Seq(Schedule.Scheme(
              AdmissionTimeScheme(Seq(DailyPeriod.always)),
              Schedule.Continuous(pause = 0.s, limit = Some(Int.MaxValue))))),
            cycleWorkflow = Workflow.empty)),
        timeZone = Timezone(zone.getId)),
      clock)

    val initialCycleState = CycleState(
      end = Timestamp("2021-09-30T21:00:00Z"),
      schemeIndex = 0,
      index = 1,
      next = Timestamp.Epoch)
    stepper.step()
    assert(stepper.events == Seq(OrderCyclingPrepared(initialCycleState)))
    assert(stepper.order == readyOrder.copy(
      state = BetweenCycles(Some(initialCycleState))))

    for (i <- 1 to 3) withClue(s"#$i ") {
      // Endless empty loop. Caller must detect this (see test below)!
      stepper.step()
      assert(stepper.events == Seq(OrderCycleStarted))
      assert(stepper.order == readyOrder.withPosition(
        Position(0) / BranchId.cycle(initialCycleState.copy(index = i)) % 0))

      assert(stepper.nextInstruction == ImplicitEnd())
      assert(stepper.nextPosition == Right(None))

      stepper.step()
      assert(stepper.events == Seq(OrderCycleFinished(Some(
        initialCycleState.copy(
          index = i + 1,
          next = Timestamp.Epoch/*immediately*/)))))
      assert(stepper.order == readyOrder
        .withPosition(Position(0))
        .copy(
          state = BetweenCycles(Some(initialCycleState.copy(
            index = i + 1)))))
      clock += 1.h
    }
    // TODO Detect this empty loop? => OrderFailedIntermediate_
  }

  "Endless Loop with Pause" in {
    val clock = TestWallClock(Timestamp("2021-09-29T21:00:00Z"))
    val stepper = new Stepper(
      Workflow(
        workflowId,
        Seq(
          Cycle(
            Schedule(Seq(
              Schedule.Scheme(
                AdmissionTimeScheme(Seq(DailyPeriod.always)),
                Schedule.Continuous(pause = 60.s)))),
            Workflow.empty)),
          timeZone = Timezone(zone.getId)),
      clock)

    stepper.step()
    val initialCycleState = CycleState(
      end = Timestamp("2021-09-30T21:00:00Z"),
      schemeIndex = 0,
      index = 1,
      next = Timestamp.Epoch)
    assert(stepper.events == Seq(OrderCyclingPrepared(initialCycleState)))
    assert(stepper.order == readyOrder
      .withPosition(Position(0))
      .copy(state = BetweenCycles(Some(initialCycleState))))

    var nextString = ""
    for (i <- 1 to 3) withClue(s"#$i ") {
      stepper.step()
      assert(stepper.events == Seq(OrderCycleStarted))
      assert(stepper.order == readyOrder
        .withPosition(Position(0) /
          s"cycle+end=${initialCycleState.end.toEpochMilli},i=$i$nextString" % 0)
        .copy(state = Order.Ready))

      stepper.step()
      assert(stepper.events == Seq(OrderCycleFinished(Some(initialCycleState.copy(
        index = i + 1,
        next = clock.now() + 60.s)))))
      assert(stepper.order == readyOrder
        .withPosition(Position(0))
        .copy(
          state = BetweenCycles(Some(initialCycleState.copy(
            schemeIndex = 0,
            index = i + 1,
            next = clock.now() + 60.s)))))

      stepper.step()
      assert(stepper.events == Nil)

      clock += 59.s
      stepper.step()
      assert(stepper.events == Nil)

      clock += 1.s

      nextString = ",next=" + clock.now().toEpochMilli
    }
  }

  "Loop with TimeLimit and Pause" in {
    val start = Timestamp("2021-09-29T21:30:00Z")  // Half past midnight in Mariehamn
    val clock = TestWallClock(start)
    val stepper = new Stepper(
      Workflow(
        workflowId,
        Seq(
          Cycle(
            Schedule(Seq(
              Schedule.Scheme(
                AdmissionTimeScheme(Seq(DailyPeriod(9*3600, 3.h))),
                Schedule.Continuous(pause = 1.h)))),
            cycleWorkflow = Workflow.empty)),
        timeZone = Timezone(zone.getId)),
      clock)

    stepper.step()

    val cycleState = CycleState(
      end = Timestamp("2021-09-30T21:00:00Z"),  // Midnight in Mariehamn
      schemeIndex = 0,
      index = 1,
      next = Timestamp("2021-09-30T06:00:00Z"))
    assert(stepper.events == Seq(OrderCyclingPrepared(cycleState)))
    assert(stepper.order == readyOrder
      .withPosition(Position(0))
      .copy(state = BetweenCycles(Some(cycleState))))

    stepper.step()
    assert(stepper.events.isEmpty)

    clock += 9.h - 30*60.s/*because it was half past midnight*/

    val n = 3
    var i = 1
    while (i <= n) withClue(s"#$i ") {
      stepper.step()
      assert(stepper.events == Seq(OrderCycleStarted))
      assert(stepper.order == readyOrder.withPosition(Position(0) /
        s"cycle+end=1633035600000,i=$i,next=${clock.now().toEpochMilli}" % 0))

      stepper.step()
      i += 1
      val next = (i <= n) ? (clock.now() + 1.h)
      assert(stepper.events == Seq(OrderCycleFinished(
        for (next <- next) yield cycleState.copy(
          index = i,
          next = next))))
      assert(stepper.order == readyOrder
        .copy(
          state = BetweenCycles(
            next.map(next => cycleState.copy(
              schemeIndex = 0,
              index = i,
              next = next)))))

      if (next.isDefined) {
        stepper.step()
        assert(stepper.events == Nil)
        clock += 1.h
      }
    }

    stepper.step()
    assert(stepper.events == Seq(OrderMoved(Position(1))))
    assert(stepper.order == readyOrder
      .withPosition(Position(1))
      .copy(state = Order.Ready))
  }

  "Mariehamn daylight saving time (to be sure)" in {
    assert(local("2020-10-25T00:00") == Timestamp("2020-10-24T21:00:00Z"))
    assert(local("2020-10-25T03:00") == Timestamp("2020-10-25T00:00:00Z"))
    assert(local("2020-10-25T03:30") == Timestamp("2020-10-25T00:30:00Z"))
    assert(local("2020-10-25T04:00") == Timestamp("2020-10-25T02:00:00Z"))
    assert(local("2020-10-25T04:30") == Timestamp("2020-10-25T02:30:00Z"))
    assert(local("2020-10-26T00:00") == Timestamp("2020-10-25T22:00:00Z"))
    assert(local("2020-10-25T04:00") - local("2020-10-25T03:00") == 2.h)
    assert(local("2020-10-25T04:00") - local("2020-10-25T03:59:59") == 1.h + 1.s)

    assert(local("2021-03-28T02:59") == Timestamp("2021-03-28T00:59:00Z"))
    assert(local("2021-03-28T04:00") == Timestamp("2021-03-28T01:00:00Z"))
    assert(local("2021-03-28T04:00") - local("2021-03-28T02:59:59") == 1.s)
  }

  "Daylight saving time change" in {
    pending  // TODO

    val clock = TestWallClock(local("2020-10-25T02:30"))
    val stepper = new Stepper(
      Workflow(
        workflowId,
        Seq(
          Cycle(
            Schedule(Seq(Scheme(
              AdmissionTimeScheme(Seq(
                AlwaysPeriod)),
              Periodic(1.h, Seq(0.minute, 30.minute))))),
            Workflow.empty)),
        timeZone = Timezone(zone.getId)),
      clock)

    stepper.step()
    val initialCycleState = CycleState(
      end = local("2020-10-26T00:00"),
      schemeIndex = 0,
      index = 1,
      next = Timestamp("2020-10-24T23:30:00Z"))
    assert(stepper.events == Seq(OrderCyclingPrepared(initialCycleState)))
    assert(stepper.order == readyOrder
      .withPosition(Position(0))
      .copy(state = BetweenCycles(Some(initialCycleState))))

    for (i <- 1 to 3) withClue(s"#$i ") {
      stepper.step()
      assert(stepper.events == Seq(OrderCycleStarted))

      stepper.step()
      assert(stepper.events == Seq(OrderCycleFinished(Some(initialCycleState.copy(
        index = i + 1,
        next = clock.now() + 60.s)))))

      stepper.step()
      assert(stepper.events == Nil)

      clock += 59.s
      stepper.step()
      assert(stepper.events == Nil)

      clock += 1.s
    }
  }

  "CycleTest example" - {
    addStandardCycleTests { (start, cycleDuration, zone, expected, exitTimestamp) =>
      val expectedCycleStartTimes = expected
        .map { case (cycleWaitTimestamp, cycleState) =>
          cycleWaitTimestamp max cycleState.next  // Expected time of OrderCycleStart
        }
      assert(testCycle(start, cycleDuration, zone) == expectedCycleStartTimes -> exitTimestamp)
    }

    def testCycle(start: Timestamp, cycleDuration: FiniteDuration, zone: ZoneId)
    : (Seq[Timestamp], Timestamp) = {
      val clock = TestWallClock(start)
      val stepper = new Stepper(
        Workflow(workflowId, Seq(CycleTest.exampleCycle), timeZone = Timezone(zone.getId)),
        clock)
      val builder = new VectorBuilder[Timestamp]

      var i = 1
      while (!stepper.order.isState[Finished] && i <= 10000) withClue(s"#i") {
        i += 1
        stepper.step()
        assert(stepper.events.nonEmpty)
        if (stepper.events.contains(OrderCycleStarted)) {
          builder += clock.now()

          // Time to execute the instruction block
          clock += cycleDuration
        } else
          for (
            order <- stepper.order.ifState[BetweenCycles];
            cycleState <- order.state.cycleState)
          {
            if (cycleState.next > clock.now()) {
              // Cycle is delayed, so we adjust the clock
              clock := cycleState.next
            }
          }
      }
      builder.result() -> clock.now()
    }
  }

  "Second OrderCyclingPrepared due to late continuation" - {
    lazy val workflow = Workflow(
      workflowId,
      Seq(
        Cycle(
          Schedule(Seq(
            Scheme(
              AdmissionTimeScheme(Seq(DailyPeriod(LocalTime.parse("07:00"), 1.h))),
              Ticking(15.minutes)),
            Scheme(
              AdmissionTimeScheme(Seq(DailyPeriod(LocalTime.parse("12:00"), 1.h))),
              Periodic(1.h, Seq(20.minute))))),
          cycleWorkflow = Workflow.empty)),
      timeZone = Timezone(zone.getId))

    lazy val stateView = new StateView.ForTest {
      def isAgent = true

      override def idToWorkflow = Map(workflow.id -> workflow)
    }

    def executorService(ts: Timestamp) =
      new InstructionExecutorService(WallClock.fixed(ts))

    lazy val order: Order[Order.State] = readyOrder.copy(
      state = BetweenCycles(Some(CycleState(
        end = local("2021-10-02T00:00"),
        next = local("2021-10-01T07:44"),
        index = 1,
        schemeIndex = 0))))

    "now < next => OrderCylceStarted" in {
      assert(executorService(local("2021-10-01T07:44")).toEvents(order, stateView) ==
        Right(List(order.id <-: OrderCycleStarted)))
    }

    "now == next => OrderCylceStarted" in {
      assert(executorService(local("2021-10-01T07:45")).toEvents(order, stateView) ==
        Right(List(order.id <-: OrderCycleStarted)))
    }

    "now > next => OrderCylceStarted" in {
      assert(executorService(local("2021-10-01T07:46")).toEvents(order, stateView) ==
        Right(List(order.id <-: OrderCycleStarted)))
    }

    "now >= end => OrderCyclingPrepared to change the scheme" in {
      assert(executorService(local("2021-10-01T08:00")).toEvents(order, stateView) ==
        Right(List(order.id <-: OrderCyclingPrepared(CycleState(
          end = local("2021-10-02T00:00"),
          next = local("2021-10-01T12:20"),
          index = 1,
          schemeIndex = 1)))))
    }

    "Same with Stepper" in {
      val clock = TestWallClock(local("2021-10-01T07:46"))
      val stepper = new Stepper(workflow, clock)

      stepper.step()
      assert(stepper.events == Seq(OrderCyclingPrepared(CycleState(
        end = local("2021-10-02T00:00"),
        next = local("2021-10-01T07:00"),
        index = 1,
        schemeIndex = 0))))

      stepper.step()
      assert(stepper.events == Seq(OrderCycleStarted))
      stepper.step()
      assert(stepper.events == Seq(OrderCycleFinished(Some(CycleState(
        end = local("2021-10-02T00:00"),
        next = local("2021-10-01T07:45"),
        index = 2,
        schemeIndex = 0)))))

      // Reaching and of AdmissionTimeScheme
      clock := local("2021-10-01T08:00")
      stepper.step()
      assert(stepper.events == Seq(OrderCyclingPrepared(CycleState(
        end = local("2021-10-02T00:00"),
        next = local("2021-10-01T12:20"),
        index = 1,
        schemeIndex = 1))))
    }
  }
}

object CycleExecutorTest
{
  private val logger = Logger[this.type]
  private val workflowId = WorkflowPath("WORKFLOW") ~ "1"

  private val readyOrder = Order(
    OrderId("ORDER"),
    (WorkflowPath("WORKFLOW") ~ "1") /: Position(0),
    Ready)

  final class Stepper(workflow: Workflow, val clock: WallClock)
  {
    private lazy val stateView = new StateView.ForTest {
      def isAgent = true

      override def idToWorkflow = Map(workflow.id -> workflow)
    }

    private val executorService = new InstructionExecutorService(clock)
    var order: Order[Order.State] = readyOrder
    var events: Seq[OrderEvent] = Nil

    logger.debug("—" * 80)

    def nextInstruction =
      stateView.instruction(order.workflowPosition)

    def nextPosition =
      executorService.nextPosition(nextInstruction, order, stateView)

    def step() = {
      val keyedEvents = executorService.toEvents(nextInstruction, order, stateView).orThrow
      for (ke <- keyedEvents) logger.debug(s"${clock.now()} $ke")
      assert(keyedEvents.forall(_.key == order.id))
      val events = keyedEvents.map(_.event)
      order = order.applyEvents(events).orThrow
      this.events = events
    }
  }
}
