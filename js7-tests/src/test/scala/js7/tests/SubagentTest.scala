package js7.tests

import cats.effect.Resource
import java.nio.file.Files.{createDirectories, createDirectory}
import java.nio.file.Path
import js7.agent.RunningAgent
import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.Outcome.Disrupted.ProcessLost
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentRefStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated}
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.proxy.ControllerApi
import js7.subagent.BareSubagent
import js7.subagent.configuration.SubagentConf
import js7.tests.SubagentTest._
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View
import scala.util.control.NonFatal

final class SubagentTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    """
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, cWorkflow, bSubagentRef)

  private lazy val bSubagentPort = findFreeTcpPort()
  private lazy val bSubagentRef = SubagentRef(
    SubagentId("B-SUBAGENT"),
    agentPath,
    Uri(s"http://localhost:$bSubagentPort"))

  private implicit val scheduler = Scheduler.global

  private lazy val controller: RunningController = directoryProvider
    .startController()
    .await(99.s)

  protected lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(Admission(
      controller.localUri,
      Some(directoryProvider.controller.userAndPassword)
    )))(controller.actorSystem))

  private var agent: RunningAgent = null
  private lazy val aSubagentId = directoryProvider.subagentId
  private var bSubagent: BareSubagent = null
  private var bSubagentRelease = Task.unit

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

  import controller.eventWatch

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
    Uri(s"http://localhost:${findFreeTcpPort()}"),
    priority = Some(1)/*higher than None*/)

  "Add and use a prioritized Subagent" in {
    TestSemaphoreJob.reset()
    val eventId = eventWatch.lastAddedEventId

    controllerApi.updateUnsignedSimpleItems(Seq(cSubagentRef)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == cSubagentRef.id)

    runSubagent(cSubagentRef) { _ =>
      eventWatch.await[SubagentCoupled](_.key == cSubagentRef.id, after = eventId)
      for (i <- 1 to 2) {
        val orderId = OrderId(s"PRIORITIZED-SUBAGENT-$i")
        TestSemaphoreJob.continue()
        controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
        val events = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        assert(events.head.value == orderId <-: OrderProcessingStarted(cSubagentRef.id))
        eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
      }
    }.await(199.s)
    eventWatch.await[SubagentCouplingFailed](_.key == cSubagentRef.id, after = eventId)

    // Now, the another available Subagent is selected
    val orderId = OrderId("NEXT-PRIORITIZED-SUBAGENT")
    TestSemaphoreJob.reset()
    TestSemaphoreJob.continue()
    controller.addOrder(FreshOrder(orderId, cWorkflow.path)).await(99.s).orThrow
    val events = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
    assert(events.head.value == orderId <-: OrderProcessingStarted(aSubagentId))
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }

  "Restart Director" in {
    val orderId = OrderId("RESTART-DIRECTOR")

    runSubagent(cSubagentRef) { _ =>
      locally {
        val eventId = eventWatch.lastAddedEventId
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

    locally {
      // Disable local Subagent. We want use only cSubagentRef
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

    val aOrderId = OrderId("A-RESTART-SUBAGENT")

    TestSemaphoreJob.reset()

    runSubagent(cSubagentRef) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, cWorkflow.path)).await(99.s).orThrow
      val events = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
      assert(events.head.value.event == OrderProcessingStarted(cSubagentRef.id))
      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      //? sleep(1.s)  // JobDriver.stop still races with .processOrder(). Here we give processOrder() a second.
    }.await(199.s)

    // Subagent is unreachable now
    eventId = eventWatch.lastAddedEventId
    val bOrderId = OrderId("B-RESTART-SUBAGENT")
    controller.addOrder(FreshOrder(bOrderId, cWorkflow.path)).await(99.s).orThrow
//    //??? Because heartbeat timeout has still not elapsed, the Subagent appears to be alive and
//    // OrderProcessingStarted is being emitted.
//    // eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)

    eventId = eventWatch.lastAddedEventId
    runSubagent(cSubagentRef) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))
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

    var eventId = eventWatch.lastAddedEventId
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

  "Change JobResource" in {
    pending // TODO
  }

  "Delete Subagent while processes are still running" in {
    pending // TODO
  }

  private def runSubagent[A](subagentRef: SubagentRef, awaitDedicated: Boolean = true)
    (body: BareSubagent => A)
  : Task[A] =
    Task.defer {
      val eventId = eventWatch.lastAddedEventId
      subagentResource(subagentRef).use { subagent =>
        if (awaitDedicated) eventWatch.await[SubagentDedicated](after = eventId)
        Task {
          try body(subagent)
          catch { case NonFatal(t) =>
            logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
            throw t
          }
        }
      }
    }

  private def subagentResource(subagentRef: SubagentRef): Resource[Task, BareSubagent] =
    for {
      dir <- subagentEnvironment(subagentRef)
      conf = toSubagentConf(dir, subagentRef.uri.port.orThrow, name = subagentRef.id.string)
      scheduler <- BareSubagent.threadPoolResource[Task](conf)
      subagent <- BareSubagent.resource(conf.finishAndProvideFiles, scheduler)
    } yield subagent

  private def subagentEnvironment(subagentRef: SubagentRef): Resource[Task, Path] =
    Resource.make(
      acquire = Task {
        val dir = directoryProvider.directory / "subagents" / subagentRef.id.string
        createDirectories(directoryProvider.directory / "subagents")
        createDirectory(dir)
        createDirectory(dir / "data")
        createDirectory(dir / "data" / "logs")
        dir
      })(
      release = dir => Task {
        deleteDirectoryRecursively(dir)
      })

  private def toSubagentConf(directory: Path, port: Int, name: String): SubagentConf =
    SubagentConf.of(
      configDirectory = directory / "config",
      dataDirectory = directory / "data",
      Seq(WebServerPort.localhost(port)),
      name = s"SubagentTest-$name",
      config = config"""
        js7.job.execution.signed-script-injection-allowed = yes
        js7.auth.users.AGENT-0 {
          permissions: [ AgentDirector ]
          password: ""
        }
      """)
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
      TestSemaphoreJob.execute(agentPath)))

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
