package js7.tests

import java.util.concurrent.TimeoutException
import js7.agent.RunningAgent
import js7.base.Problems.MessageSignedByUnknownProblem
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.Outcome.Disrupted.ProcessLost
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentDedicated}
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.subagent.BareSubagent
import js7.tests.SubagentTest._
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View

final class SubagentTest extends AnyFreeSpec
with DirectoryProviderForScalaTest
with SubagentTester
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.auth.subagents.AGENT-1 = "AGENT-PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    js7.auth.subagents.C-SUBAGENT = "AGENT-PASSWORD"
    """
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, cWorkflow, bSubagentRef)

  private lazy val bSubagentPort = findFreeTcpPort()
  private lazy val bSubagentRef = SubagentRef(
    SubagentId("B-SUBAGENT"),
    agentPath,
    Uri(s"http://localhost:$bSubagentPort"))

  protected implicit val scheduler = Scheduler.global

  private var agent: RunningAgent = null
  private lazy val aSubagentId = directoryProvider.subagentId
  private var bSubagent: BareSubagent = null
  private var bSubagentRelease = Task.unit

  import controller.eventWatch

  override def beforeAll() = {
    super.beforeAll()
    agent = directoryProvider.startAgent(agentPath).await(99.s)
    controller
  }

  override def afterAll() = {
    controllerApi.stop.await(99.s)
    controller.terminate().await(99.s)
    for (a <- Option(agent)) a.terminate().await(99.s)
    bSubagentRelease.await(99.s)
    super.beforeAll()
  }

  "Start a second Subagent" in {
    val eventId = eventWatch.lastAddedEventId
    val pair = subagentResource(bSubagentRef).allocated.await(99.s)
    bSubagent = pair._1
    bSubagentRelease = pair._2
    eventWatch.await[SubagentDedicated](_.key == bSubagentRef.id, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == bSubagentRef.id, after = eventId)
  }

  "Multiple orders" in {
    val n = 100
    val runOrders = runMultipleOrders(
      orderIds = for (i <- 1 to n) yield OrderId(s"ORDER-$i"),
      assertEvents = (orderId, events) =>
        Task {
          val result = withClue(s"$orderId: ") {
            val anySubagentId = SubagentId("ANY")
            assert(events.collect {
              case OrderProcessingStarted(_) => OrderProcessingStarted(anySubagentId)
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
              OrderFinished))
          }
          logger.info(s"$orderId ✔︎")
          result
        })

    runOrders.await(99.s)
    assert(eventWatch.allKeyedEvents[OrderProcessingStarted].map(_.event.subagentId).toSet == Set(
      Some(aSubagentId),
      Some(bSubagentRef.id)))
  }

  private def runMultipleOrders(
    orderIds: Iterable[OrderId],
    assertEvents: (OrderId, Seq[OrderEvent]) => Task[Assertion])
  : Task[Unit] =
    controllerApi
      .addOrders(Observable
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
  : Observable[(OrderId, Seq[OrderEvent])] =
    controllerApi
      .eventAndStateObservable(fromEventId = Some(EventId.BeforeFirst))
      .mapAccumulate(orderIds.map(_ -> Vector.empty[OrderEvent]).toMap) {
        case (idToEvents, eventAndState) =>
          eventAndState.stampedEvent match {
            case Stamped(_, _, KeyedEvent(orderId: OrderId, event: OrderEvent)) =>
              if (!orderIds.contains(orderId))
                idToEvents -> (None -> true)
              else {
                val iToE = idToEvents + (orderId -> (idToEvents(orderId) :+ event))
                event match {
                  case _: OrderFinished =>
                    val iToE2 = iToE.removed(orderId)
                    if (iToE2.isEmpty)
                      iToE2 -> (Some(orderId -> iToE(orderId)) -> false/*do not continue!*/)
                    else
                      iToE2 -> (Some(orderId -> iToE(orderId)) -> true)
                  case _ =>
                    iToE -> (None -> true)
                }
              }

            case _ =>
              idToEvents -> (None -> true)
          }
      }
      .takeWhileInclusive(_._2)
      .map(_._1)
      .flatMap(o => Observable.fromIterable(o))

  private lazy val cSubagentRef = SubagentRef(
    SubagentId("C-SUBAGENT"),
    agentPath,
    Uri(s"http://localhost:${findFreeTcpPort()}"))

  "Add C-SUBAGENT" in {
    controllerApi.updateUnsignedSimpleItems(Seq(cSubagentRef)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == cSubagentRef.id)
  }

  "Disable local Subagent" in {
    // Disable local Subagent and bSubagentRef. We want use only cSubagentRef
    val aSubagentRef = directoryProvider.subagentRefs(0)
    controllerApi
      .updateUnsignedSimpleItems(Seq(
        aSubagentRef.copy(
          disabled = true,
          itemRevision = None)))
      .await(99.s)
      .orThrow
    eventWatch.await[ItemAttached](_.event.key == aSubagentRef.id)
  }

  "Remove Subagent" - {
    "Remove bSubagent" in {
      val eventId = eventWatch.lastAddedEventId
      controllerApi
        .updateItems(Observable(
          ItemOperation.DeleteSimple(bSubagentRef.path)))
        .await(99.s)
        .orThrow
      eventWatch.await[ItemDetachable](_.event.key == bSubagentRef.id, after = eventId)
    }

    "Remove Subagent while an Order is processed" in {
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("REMOVE-SUBAGENT")
      runSubagent(cSubagentRef) { subagent =>
        controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
        val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
          .head.value.event
        assert(started == OrderProcessingStarted(cSubagentRef.path))
        eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

        controllerApi
          .updateItems(Observable(
            ItemOperation.DeleteSimple(cSubagentRef.path)))
          .await(99.s)
          .orThrow
        eventWatch.await[ItemDetachable](_.event.key == cSubagentRef.id, after = eventId)

        // ItemDetached is delayed until no Order is being processed
        intercept[TimeoutException](
          eventWatch.await[ItemDetached](_.event.key == cSubagentRef.id, after = eventId, timeout = 1.s))

        TestSemaphoreJob.continue()
        val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
          .head.value.event
        assert(processed == OrderProcessed(Outcome.succeeded))

        eventWatch.await[ItemDetached](_.event.key == cSubagentRef.id, after = eventId)
        eventWatch.await[ItemDeleted](_.event.key == cSubagentRef.id, after = eventId)
        //subagent.shutdown(signal = None).await(99.s)
        subagent.untilStopped.await(99.s)
      }.await(99.s)
    }

    // TODO Das Löschen kann dauern, wenn der Auftrag lange dauert,
    //  länger als ein HTTP-Request braucht.
    //  Also nicht auf Antwort auf AgentCommand.DetachItem warten,
    //  sondern auf ein Event: ItemDetachable (vom Agenten?)

    "Don't allow orders to start under removal" in {
      pending // TODO
    }

    "Remove Subagent and continue removal after Subagent's restart" in {
      pending // TODO
    }
  }

  "Add C-SUBAGENT again" in {
    val eventId = eventWatch.lastAddedEventId
    controllerApi.updateUnsignedSimpleItems(Seq(cSubagentRef)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == cSubagentRef.id, after = eventId)
  }

  "Reject items if no signature keys are installed" in {
    val eventId = eventWatch.lastAddedEventId

    runSubagent(cSubagentRef, suppressSignatureKeys = true) { _ =>
      val orderId = OrderId("ITEM-SIGNATURE")
      controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentRef.id))

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(Outcome.Disrupted(MessageSignedByUnknownProblem)))
    }.await(99.s)
  }

  "Restart Director" in {
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("RESTART-DIRECTOR")

    runSubagent(cSubagentRef) { _ =>
      locally {
        controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
        val events = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        assert(events.head.value.event == OrderProcessingStarted(cSubagentRef.id))

        // STOP DIRECTOR
        agent.terminate().await(99.s)
      }

      TestSemaphoreJob.continue()

      locally {
        val eventId = eventWatch.lastAddedEventId
        eventWatch.keyedEvents[OrderProcessed] foreach {
          case ke @ KeyedEvent(`orderId`, OrderProcessed(_)) => fail(s"Unexpected $ke")
          case _ =>
        }

        // START DIRECTOR
        agent = directoryProvider.startAgent(agentPath).await(99.s)
        eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
        assert(events.head.value.event.isInstanceOf[OrderFinished])
      }
    }.await(199.s)
  }

  "Restart remote Subagent while a job is running" in {
    var eventId = eventWatch.lastAddedEventId
    val aOrderId = OrderId("A-RESTART-SUBAGENT")

    TestSemaphoreJob.reset()

    runSubagent(cSubagentRef) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentRef.id))

      val written = eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(written == OrderStdoutWritten("STARTED\n"))

      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)
    }.await(199.s)

    // Subagent is unreachable now
    eventId = eventWatch.lastAddedEventId
    val bOrderId = OrderId("B-RESTART-SUBAGENT")
    controller.addOrder(FreshOrder(bOrderId, cWorkflow.path)).await(99.s).orThrow

    runSubagent(cSubagentRef) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))

        // OrderProcessed must be followed by OrderMoved
        eventWatch.await[OrderMoved](_.key == aOrderId, after = events.last.eventId)
      }
      locally {
        sleep(4.s)
        TestSemaphoreJob.continue(2)
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        for (orderId <- View(aOrderId, bOrderId)) {
          val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
          assert(events.head.value.event.isInstanceOf[OrderFinished])
        }
      }
    }
    .await(199.s)
  }

  "Restart both Director and remote Subagent while a job is running" in {
    var eventId = eventWatch.lastAddedEventId
    val aOrderId = OrderId("A-RESTART-BOTH")

    TestSemaphoreJob.reset()

    runSubagent(cSubagentRef) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentRef.id))

      val written = eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(written == OrderStdoutWritten("STARTED\n"))

      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)

      // STOP DIRECTOR
      agent.terminate().await(99.s)
    }.await(199.s)
    // Subagent is unreachable now

    // START DIRECTOR
    eventId = eventWatch.lastAddedEventId
    logger.debug(s"eventId=$eventId")
    agent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[AgentReady](after = eventId)

    val bOrderId = OrderId("B-RESTART-BOTH")
    controller.addOrder(FreshOrder(bOrderId, cWorkflow.path)).await(99.s).orThrow

    runSubagent(cSubagentRef) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))

        // OrderProcessed must be followed by OrderMoved
        eventWatch.await[OrderMoved](_.key == aOrderId, after = events.last.eventId)
      }
      locally {
        TestSemaphoreJob.continue(2)
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        for (orderId <- View(aOrderId/*, bOrderId*/)) {
          val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
          assert(events.head.value.event.isInstanceOf[OrderFinished])
        }
      }
    }
    .await(199.s)
  }

  "CancelOrder" in {
    // Local Subagent must be disabled (see test above)

    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("CANCEL-ORDER")

    TestSemaphoreJob.reset()

    runSubagent(cSubagentRef) { _ =>
      controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow

      val processingStarted = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(processingStarted == OrderProcessingStarted(cSubagentRef.id))

      val started = eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderStdoutWritten("STARTED\n"))

      controllerApi.executeCommand(CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(Outcome.killedInternal))
      eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)
    }.await(199.s)
  }

  "Change URI of bare Subagent" in pending // TODO
  "Change URI of Director" in pending // TODO
  "Change JobResource" in pending // TODO
  "Delete Subagent while processes are still running" in pending // TODO
}

object SubagentTest
{
  private val agentPath = AgentPath("AGENT")
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestJob.execute(agentPath, parallelism = 1_000_000)))

  private val cWorkflow = Workflow(
    WorkflowPath("C-WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath, parallelism = 1_000_000)))

  final class TestJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        step.outTaskObserver.send("STDOUT 1\n") >>
        step.outTaskObserver.send("STDOUT 2\n") >>
        Task.pure(Outcome.succeeded))
  }
  object TestJob extends InternalJob.Companion[TestJob]

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
