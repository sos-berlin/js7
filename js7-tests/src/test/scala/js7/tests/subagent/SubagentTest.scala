package js7.tests.subagent

import java.util.Locale.ROOT
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
import js7.data.item.BasicItemEvent.{ItemAttached, ItemAttachedToMe, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.Outcome.Disrupted.ProcessLost
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.subagent.BareSubagent
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentTest._
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
  protected lazy val items = Seq(workflow, cWorkflow, bSubagentItem)

  private lazy val bSubagentPort = findFreeTcpPort()
  private lazy val bSubagentItem = SubagentItem(
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
    val pair = directoryProvider.subagentResource(bSubagentItem).allocated.await(99.s)
    bSubagent = pair._1
    bSubagentRelease = pair._2
    eventWatch.await[SubagentDedicated](_.key == bSubagentItem.id, after = eventId)
    eventWatch.await[SubagentCoupled](_.key == bSubagentItem.id, after = eventId)
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
      Some(bSubagentItem.id)))
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

  private lazy val cSubagentItem = SubagentItem(
    SubagentId("C-SUBAGENT"),
    agentPath,
    Uri(s"http://localhost:${findFreeTcpPort()}"))

  private lazy val c1SubagentItem =
    cSubagentItem.copy(uri = Uri("http://localhost:" + findFreeTcpPort()))

  "Add C-SUBAGENT" in {
    controllerApi.updateUnsignedSimpleItems(Seq(cSubagentItem)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == cSubagentItem.id)
  }

  "Disable local Subagent" in {
    // Disable local Subagent and bSubagentItem. We want use only cSubagentItem
    val aSubagentItem = directoryProvider.subagentItems(0)
    controllerApi
      .updateUnsignedSimpleItems(Seq(
        aSubagentItem.copy(
          disabled = true,
          itemRevision = None)))
      .await(99.s)
      .orThrow
    eventWatch.await[ItemAttached](_.event.key == aSubagentItem.id)
  }

  "Remove Subagent" - {
    "Remove bSubagent" in {
      val eventId = eventWatch.lastAddedEventId
      controllerApi
        .updateItems(Observable(
          ItemOperation.DeleteSimple(bSubagentItem.path)))
        .await(99.s)
        .orThrow
      eventWatch.await[ItemDetachable](_.event.key == bSubagentItem.id, after = eventId)
    }

    "Remove Subagent while an Order is processed" in {
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("REMOVE-SUBAGENT")
      runSubagent(cSubagentItem) { subagent =>
        controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
        val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
          .head.value.event
        assert(started == OrderProcessingStarted(cSubagentItem.path))
        eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

        controllerApi
          .updateItems(Observable(
            ItemOperation.DeleteSimple(cSubagentItem.path)))
          .await(99.s)
          .orThrow
        eventWatch.await[ItemDetachable](_.event.key == cSubagentItem.id, after = eventId)

        // ItemDetached is delayed until no Order is being processed
        intercept[TimeoutException](
          eventWatch.await[ItemDetached](_.event.key == cSubagentItem.id, after = eventId, timeout = 1.s))

        TestSemaphoreJob.continue()
        val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
          .head.value.event
        assert(processed == OrderProcessed(Outcome.succeeded))

        eventWatch.await[ItemDetached](_.event.key == cSubagentItem.id, after = eventId)
        eventWatch.await[ItemDeleted](_.event.key == cSubagentItem.id, after = eventId)
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
    controllerApi.updateUnsignedSimpleItems(Seq(cSubagentItem)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == cSubagentItem.id, after = eventId)
  }

  "Reject items if no signature keys are installed" in {
    val eventId = eventWatch.lastAddedEventId

    runSubagent(cSubagentItem, suppressSignatureKeys = true) { _ =>
      val orderId = OrderId("ITEM-SIGNATURE")
      controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentItem.id))

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(Outcome.Disrupted(MessageSignedByUnknownProblem)))
    }.await(99.s)
  }

  "Restart Director" in {
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("RESTART-DIRECTOR")

    runSubagent(cSubagentItem) { _ =>
      locally {
        controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
        val events = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        assert(events.head.value.event == OrderProcessingStarted(cSubagentItem.id))

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

    runSubagent(cSubagentItem) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentItem.id))

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

    runSubagent(cSubagentItem) { _ =>
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

    runSubagent(cSubagentItem) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(cSubagentItem.id))

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

    runSubagent(cSubagentItem) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))

        // OrderProcessed must be followed by OrderMoved
        eventWatch.await[OrderMoved](_.key == aOrderId, after = events.last.eventId)
      }
      locally {
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

  "CancelOrder" in {
    // Local Subagent must be disabled (see test above)

    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("CANCEL-ORDER")

    TestSemaphoreJob.reset()

    runSubagent(cSubagentItem) { _ =>
      controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow

      val processingStarted = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(processingStarted == OrderProcessingStarted(cSubagentItem.id))

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

  "Delete Subagent while processes are still running" in pending // TODO

  //"Change URI of Director" --> See UpdateAgentRefsTest

  "Restart Subagent at another URI" in {

    // Start c0Subagent
    val (c0Subagent, cSubagentRelease) = subagentResource(cSubagentItem, awaitDedicated = false)
      .allocated.await(99.s)

    val aOrderId = OrderId("A-MOVE-SUBAGENT")
    var eventId = eventWatch.lastAddedEventId
    locally {
      controllerApi.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow
      val processingStarted = eventWatch
        .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
      assert(processingStarted == OrderProcessingStarted(cSubagentItem.id))
      eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
      // aOrderId is waiting for semaphore
    }

    eventId = eventWatch.lastAddedEventId
    //val agentEventId = agent.eventWatch.lastAddedEventId
    controllerApi.updateItems(Observable(ItemOperation.AddOrChangeSimple(c1SubagentItem)))
      .await(99.s).orThrow
    //agent.eventWatch.await[ItemAttachedToMe](_.event.item.key == c1SubagentItem.id,
    //  after = agentEventId)
    //agent.eventWatch.await[SubagentCouplingFailed](_.key == c1SubagentItem.id, after = agentEventId)

    // Start the replacing c1Subagent while the previous c0Subagent is still running
    runSubagent(c1SubagentItem, suffix = "-1") { _ =>
      val aProcessed = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId).head
      assert(aProcessed.value.event == OrderProcessed.processLost)

      // After ProcessLost at previous Subagent aOrderId restarts at current Subagent
      TestSemaphoreJob.continue(1)  // aOrder still runs on c0Subagent (but it is ignored)
      TestSemaphoreJob.continue(1)
      val a2Processed = eventWatch
        .await[OrderProcessed](_.key == aOrderId, after = aProcessed.eventId)
        .head.value.event
      assert(a2Processed == OrderProcessed(Outcome.succeeded))

      eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

      locally {
        // Start another order
        val bOrderId = OrderId("B-MOVE-SUBAGENT")
        TestSemaphoreJob.continue(1)
        controllerApi.addOrder(FreshOrder(bOrderId, cWorkflow.path)).await(99.s).orThrow
        val bStarted = eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)
          .head.value.event
        assert(bStarted == OrderProcessingStarted(c1SubagentItem.id))

        eventWatch.await[OrderStdoutWritten](_.key == bOrderId, after = eventId)

        eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId).head.value.event
        val bProcessed = eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId)
          .head.value.event
        assert(bProcessed == OrderProcessed(Outcome.succeeded))
        eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)
      }
    }.await(199.s)

    // For this test, the terminating Subagent must no emit any event before shutdown
    //c1Subagent.journal.stopEventWatch()
    c0Subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)
    cSubagentRelease.await(99.s)
  }

  "Change only URI of continuously running Subagent ❓" in {
    // TODO Test, ob die Prozesse abgebrochen und wiederholt werden.
    //  Besser, die Aufträge werden vom neuen RemoteSubagentDriver übernommen.
    pending

    // The continuously running Subagent is reachable under a changed URI
    // Orders are not affected.
    runSubagent(c1SubagentItem) { _ =>
      TestSemaphoreJob.reset()
      var eventId = eventWatch.lastAddedEventId

      locally {
        // To be safe, the current Subagent is coupled, we run a Order.
        val aOrderId = OrderId("A-CHANGE-URI")
        controllerApi.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow
        val bStarted = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
          .head.value.event
        assert(bStarted == OrderProcessingStarted(c1SubagentItem.id))

        TestSemaphoreJob.continue(1)
        eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)
      }

      // Start an Order
      val bOrderId = OrderId("B-CHANGE-URI")
      locally {
        controllerApi.addOrder(FreshOrder(bOrderId, cWorkflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch
          .await[OrderProcessingStarted](_.key == bOrderId, after = eventId).head.value.event
        assert(processingStarted == OrderProcessingStarted(c1SubagentItem.id))
        eventWatch.await[OrderStdoutWritten](_.key == bOrderId, after = eventId)
        // bOrderId is waiting for semaphore
      }

      // Change URI to upper-case
      val c2SubagentItem = c1SubagentItem.copy(uri = Uri(c1SubagentItem.uri.string.toUpperCase(ROOT)))
      assert(c2SubagentItem.uri != c1SubagentItem.uri)
      eventId = eventWatch.lastAddedEventId
      val agentEventId = agent.eventWatch.lastAddedEventId
      controllerApi.updateItems(Observable(ItemOperation.AddOrChangeSimple(c2SubagentItem)))
        .await(99.s).orThrow
      agent.eventWatch.await[ItemAttachedToMe](_.event.item.key == c2SubagentItem.id,
        after = agentEventId)
      agent.eventWatch.await[SubagentCouplingFailed](_.key == c1SubagentItem.id, after = agentEventId)

      TestSemaphoreJob.continue(1)
      val aProcessed = eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId).head
      assert(aProcessed.value.event == OrderProcessed(Outcome.succeeded))

      eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)
      assert(eventWatch
        .allKeyedEvents[OrderEvent]
        .collect { case KeyedEvent(`bOrderId`, event) => event }
        == Seq())

      locally {
        // Start another order
        val cOrderId = OrderId("C-CHANGE-URI")
        controllerApi.addOrder(FreshOrder(cOrderId, cWorkflow.path)).await(99.s).orThrow
        val bStarted = eventWatch.await[OrderProcessingStarted](_.key == cOrderId, after = eventId)
          .head.value.event
        assert(bStarted == OrderProcessingStarted(c2SubagentItem.id))

        TestSemaphoreJob.continue(1)
        eventWatch.await[OrderProcessed](_.key == cOrderId, after = eventId).head.value.event
        val bProcessed = eventWatch.await[OrderProcessed](_.key == cOrderId, after = eventId)
          .head.value.event
        assert(bProcessed == OrderProcessed(Outcome.succeeded))
        eventWatch.await[OrderFinished](_.key == cOrderId, after = eventId)
      }
    }
  }.await(99.s)

  //"Change JobResource" in --> See JobResourceAtBareSubagentTest
}

object SubagentTest
{
  val agentPath = AgentPath("AGENT")
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
        step.outTaskObserver.send("STDOUT 1\n") *>
        step.outTaskObserver.send("STDOUT 2\n") *>
        Task.pure(Outcome.succeeded))
  }
  object TestJob extends InternalJob.Companion[TestJob]

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
