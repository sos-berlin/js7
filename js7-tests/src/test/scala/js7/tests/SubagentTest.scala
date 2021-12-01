package js7.tests

import cats.effect.Resource
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentRefStateEvent.SubagentDedicated
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.subagent.StandaloneSubagent
import js7.subagent.configuration.SubagentConf
import js7.tests.SubagentTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec

final class SubagentTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER {
      permissions = [ UpdateItem ]
    }"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    """
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, subagentRef1)

  private lazy val subagentPort = findFreeTcpPort()
  private lazy val subagentRef1 = SubagentRef(
    SubagentId("SUBAGENT-1"),
    agentPath,
    Uri(s"http://localhost:$subagentPort"))

  // FIXME Doppelt mit testResource
  private val subagent1Resource = Resource.suspend(Task {
    val name = subagentRef1.id.string
    val subagentDir = directoryProvider.directory / name
    StandaloneSubagent.resource(SubagentConf.of(
      configDirectory = subagentDir / "config",
      dataDirectory = subagentDir / "data",
      webServerPorts = Seq(WebServerPort.localhost(subagentPort)),
      config = ConfigFactory.empty,
      name = name))
  })

  //private lazy val (subagent1, subagent1Release) = subagent1Resource.allocated.await(99.s)
  //
  //override def beforeAll() = {
  //  super.beforeAll()
  //  subagent1
  //  eventWatch.await[SubagentDedicated]()
  //}
  //
  //override def afterAll(): Unit = {
  //  subagent1Release.await(99.s)
  //  super.afterAll()
  //}

  "test" in {
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
          logger.info(s"$orderId ✔️")
          result
        })

    testResource(directoryProvider.directory / "subagent", subagentPort)
      .use { subagent =>
        eventWatch.await[SubagentDedicated]()
        runOrders.executeOn(implicitly[Scheduler]) >>
          Task {
            assert(eventWatch.allKeyedEvents[OrderProcessingStarted].map(_.event.subagentId).toSet
              == Set(directoryProvider.subagentId, subagentRef1.id))
          }
      }
    //runWithOwnScheduler(directoryProvider.directory / "subagent", subagentPort)(runOrders)
      .await(99.s)
  }

  "Changing JobResource" in {
    pending // TODO
  }

  "Delete Subagent while processes are still running" in {
    pending // TODO
  }

  private def testResource(directory: Path, port: Int): Resource[Task, StandaloneSubagent] =
    for {
      conf <- subagentEnvironment(directory, port)
      scheduler <- StandaloneSubagent.threadPoolResource(conf)
      subagent <- StandaloneSubagent.resource(conf)(scheduler)
    } yield subagent

  private def subagentEnvironment(directory: Path, port: Int): Resource[Task, SubagentConf] =
    Resource.make(
      acquire = Task {
        createSubagentDirectories(directory)
        toSubagentConf(directory, port = port)
      })(
      release = _ => Task {
        deleteDirectoryRecursively(directory)
      }
    )

  private def toSubagentConf(directory: Path, port: Int): SubagentConf =
    SubagentConf.of(
      configDirectory = directory / "config",
      dataDirectory = directory / "data",
      Seq(WebServerPort.localhost(port)),
      name = "SUBAGENT-1",
      config = config"""
        js7.job.execution.signed-script-injection-allowed = yes
        js7.auth.users.AGENT-0 {
          permissions: [ AgentDirector ]
          password: ""
        }
      """)

  private def createSubagentDirectories(dir: Path): Unit = {
    createDirectory(dir)
    //createDirectory(dir / "config")
    createDirectory(dir / "data")
    createDirectory(dir / "data" / "logs")
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
          .mapParallelUnordered(sys.runtime.availableProcessors) {
            case (orderId, events) =>
              assertEvents(orderId, events)
                .as(orderId)
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
}

object SubagentTest
{
  private val agentPath = AgentPath("AGENT")
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestJob.execute(agentPath, parallelism = 1_000_000)))

  final class TestJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        // FIXME? Delay before writing to out because early OrderStdWritten may get lost
        //Task.sleep(Random.nextInt(4) * 500.ms + 500.ms) >>
        step.outTaskObserver.send("STDOUT 1\n") >>
        step.outTaskObserver.send("STDOUT 2\n") >>
        Task.pure(Outcome.succeeded))
  }
  object TestJob extends InternalJob.Companion[TestJob]
}
