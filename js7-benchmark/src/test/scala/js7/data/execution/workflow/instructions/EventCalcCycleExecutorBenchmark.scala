package js7.data.execution.workflow.instructions

import js7.base.crypt.{GenericSignature, Signed, SignedString}
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AdmissionTimeScheme, WallClock}
import js7.base.utils.ScalaUtils.syntax.*
import js7.benchmark.OurBenchmark
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, TimeCtx}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.Schedule.Scheme
import js7.data.workflow.instructions.{Cycle, Schedule}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Param, Warmup}
import scala.compiletime.uninitialized

/** Benchmark for EventColl, using CycleExecutor as an example.
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.data.execution.workflow.instructions.EventCalcCycleExecutorBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 3)
@Measurement(time = 1, iterations = 3)
@Fork(1)
class EventCalcCycleExecutorBenchmark extends OurBenchmark:

  private val orderId = OrderId("#2024-12-30#")

  private val cycle = Cycle(Schedule(Seq(Scheme(
    AdmissionTimeScheme.allDay,
    Schedule.Continuous(0.s, limit = Some(2)))
  ))):
    Workflow.empty

  private val versionId = VersionId("1")

  private val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"))

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ versionId,
    Seq(cycle),
    calendarPath = Some(calendar.path))

  private val order =
    Order(orderId, workflow.id /: Position(0), Order.Ready())

  private val controllerState =
    ControllerState.empty
      .copy(idToOrder = Map(order.id -> order))
      .applyKeyedEvents(Seq(
        NoKey <-: UnsignedSimpleItemAdded(calendar.copy(itemRevision = Some(ItemRevision(1)))),
        NoKey <-: VersionAdded(versionId),
        NoKey <-: VersionedItemAdded(Signed(
          workflow,
          SignedString("???", GenericSignature("???", "???"))))))
      .orThrow

  private val myOldCycleExecutor = CycleExecutor(InstructionExecutorService(WallClock))

  @Param(Array(1_000_000.toString))
  private var size: Int = uninitialized

  @Benchmark
  def oldCycleExecutor: (Seq[AnyKeyedEvent], ControllerState) =
    val keyedEvents = myOldCycleExecutor.toEvents(cycle, order, controllerState)
      .orThrow
    keyedEvents -> controllerState.applyKeyedEvents(keyedEvents).orThrow

  private val myTestCycleExecutor = EventCalcCycleExecutor[ControllerState]

  @Benchmark
  def eventCalcCycleExecutor: (Seq[AnyKeyedEvent], ControllerState) =
    val coll = myTestCycleExecutor.toEventCalc(orderId)
      .calculate(controllerState, TimeCtx(ts"2025-03-14T12:00:00Z"))
      .orThrow
    coll.keyedEvents -> coll.aggregate


//object EventCalcCycleExecutorBenchmark:
//  def main(args: Array[String]): Int=
//    OurBenchmark.runBenchmark(classOf[EventCalcCycleExecutorBenchmark])
//    0
