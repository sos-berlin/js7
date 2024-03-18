package js7.tests.subagent

import cats.effect.IO
import fs2.Stream
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.Tests
import js7.base.utils.Tests.isIntelliJIdea
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
import org.scalatest.Assertion

final class SubagentMultipleOrdersTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)

  private lazy val localSubagentId = directoryProvider.subagentId

  "Multiple orders" in:
    runSubagent(bareSubagentItem) { _ =>
      val n = if isIntelliJIdea then 20000 else 100
      val runOrders = runMultipleOrders(
        orderIds = for i <- 1 to n yield OrderId(s"ORDER-$i"),
        assertEvents = (orderId, events) =>
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
          result)

      runOrders.await(99.s)
      assert(eventWatch.allKeyedEvents[OrderProcessingStarted].map(_.event.subagentId).toSet == Set(
        Some(localSubagentId),
        Some(bareSubagentItem.id)))
    }

  private def runMultipleOrders(
    orderIds: Iterable[OrderId],
    assertEvents: (OrderId, Seq[OrderEvent]) => Assertion)
  : IO[Unit] =
    controller.api
      .addOrders(Stream
        .iterable(orderIds)
        .map(FreshOrder(_, workflow.path)))
      .map(_.orThrow)
      .flatMap: _ =>
        observeFinishedOrderEvents(orderIds.toSet)
          .mapParallelBatch(): (orderId, events) =>
            assertEvents(orderId, events)
            orderId
          .compile.to(Set)
      .map(observedOrderIds => assert(observedOrderIds == orderIds.toSet))

  private def observeFinishedOrderEvents(orderIds: Set[OrderId])
  : Stream[IO, (OrderId, Seq[OrderEvent])] =
    controller.api
      .eventAndStateStream(fromEventId = Some(EventId.BeforeFirst))
      .mapAccumulate(orderIds.map(_ -> Vector.empty[OrderEvent]).toMap):
        (idToEvents, eventAndState) =>
          eventAndState.stampedEvent.value match
            case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
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
      .takeThrough(_._2._2)
      .map(_._2._1)
      .flatMap(Stream.iterable)


object SubagentMultipleOrdersTest:
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestJob.execute(agentPath, processLimit = 1_000_000)))

  final class TestJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess(
        step.writeOut("STDOUT 1\n") *>
        step.writeOut("STDOUT 2\n") *>
        IO.pure(Outcome.succeeded))
  object TestJob extends InternalJob.Companion[TestJob]
