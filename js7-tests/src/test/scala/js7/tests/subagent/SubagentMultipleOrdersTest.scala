package js7.tests.subagent

import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixStream
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.subagent.SubagentMultipleOrdersTest.*
import js7.tests.subagent.SubagentTester.agentPath
import cats.effect.IO
import monix.execution.Scheduler
import fs2.Stream
import org.scalatest.Assertion

final class SubagentMultipleOrdersTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)

  protected implicit val scheduler = Scheduler.traced

  private lazy val localSubagentId = directoryProvider.subagentId

  "Multiple orders" in:
    runSubagent(bareSubagentItem) { _ =>
      val n = 100
      val runOrders = runMultipleOrders(
        orderIds = for i <- 1 to n yield OrderId(s"ORDER-$i"),
        assertEvents = (orderId, events) =>
          IO {
            val result = withClue(s"$orderId: ") {
              val anySubagentId = SubagentId("ANY")
              assert(events.collect {
                case OrderProcessingStarted(_, false) => OrderProcessingStarted(anySubagentId)
                case o => o
              } == Seq(
                OrderAdded(workflow.id),
                OrderAttachable(agentPath),
                OrderAttached(agentPath),
                OrderStarted,
                OrderProcessingStarted(anySubagentId),
                OrderStdoutWritten("STDOUT 1\nSTDOUT 2\n"),
                OrderProcessed(Outcome.succeeded),
                OrderMoved(Position(1)),
                OrderDetachable,
                OrderDetached,
                OrderFinished()))
            }
            logger.info(s"$orderId ✔︎")
            result
          })

      runOrders.await(99.s)
      assert(eventWatch.allKeyedEvents[OrderProcessingStarted].map(_.event.subagentId).toSet == Set(
        Some(localSubagentId),
        Some(bareSubagentItem.id)))
    }

  private def runMultipleOrders(
    orderIds: Iterable[OrderId],
    assertEvents: (OrderId, Seq[OrderEvent]) => IO[Assertion])
  : IO[Unit] =
    controller.api
      .addOrders(Stream
        .fromIterable(orderIds)
        .map(FreshOrder(_, workflow.path)))
      .map(_.orThrow)
      .flatMap(_ =>
        observeFinishedOrderEvents(orderIds.toSet)
          .mapParallelUnordered(sys.runtime.availableProcessors) { case (orderId, events) =>
            assertEvents(orderId, events).as(orderId)
          }
          .toL(Set))
      .map(observedOrderIds => assert(observedOrderIds == orderIds.toSet) )

  private def observeFinishedOrderEvents(orderIds: Set[OrderId])
  : Stream[IO, (OrderId, Seq[OrderEvent])] =
    controller.api
      .eventAndStateStream(fromEventId = Some(EventId.BeforeFirst))
      .mapAccumulate(orderIds.map(_ -> Vector.empty[OrderEvent]).toMap):
        case (idToEvents, eventAndState) =>
          eventAndState.stampedEvent match
            case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
              if !orderIds.contains(orderId) then
                idToEvents -> (None -> true)
              else
                val iToE = idToEvents + (orderId -> (idToEvents(orderId) :+ event))
                event match
                  case _: OrderFinished =>
                    val iToE2 = iToE.removed(orderId)
                    if iToE2.isEmpty then
                      iToE2 -> (Some(orderId -> iToE(orderId)) -> false/*do not continue!*/)
                    else
                      iToE2 -> (Some(orderId -> iToE(orderId)) -> true)
                  case _ =>
                    iToE -> (None -> true)

            case _ =>
              idToEvents -> (None -> true)
      .takeWhileInclusive(_._2)
      .map(_._1)
      .flatMap(o => Stream.fromIterable(o))


object SubagentMultipleOrdersTest:
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestJob.execute(agentPath, processLimit = 1_000_000)))

  final class TestJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess(
        step.outIOObserver.send("STDOUT 1\n") *>
        step.outIOObserver.send("STDOUT 2\n") *>
        IO.pure(Outcome.succeeded))
  object TestJob extends InternalJob.Companion[TestJob]
