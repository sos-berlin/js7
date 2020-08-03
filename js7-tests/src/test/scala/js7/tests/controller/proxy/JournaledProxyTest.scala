package js7.tests.controller.proxy

import io.circe.Encoder
import io.circe.syntax._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.circeutils.CirceUtils._
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.base.time.{Stopwatch, Timestamp}
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.client.AkkaHttpControllerApi
import js7.data.agent.AgentRefPath
import js7.data.controller.ControllerFileBaseds.jsonCodec
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.filebased.{FileBased, UpdateRepoOperation, VersionId}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.ControllerApi
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.javaapi.{JAdmission, JCredentials}
import js7.tests.controller.proxy.JournaledProxyTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._

final class JournaledProxyTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateRepo ]
      }
    }
    """

  protected val fileBased = workflow :: Nil
  protected val agentRefPaths = agentRefPath :: Nil

  override def beforeAll() = {
    super.beforeAll()
    (directoryProvider.controller.configDir / "private" / "private.conf") ++= """
      |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
      |""".stripMargin
    directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), script(1.s))
  }

  "JournaledProxy[ControllerState]" in {
    directoryProvider.run { (controller, _) =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(controller.localUri, Some(userAndPassword), name = "JournaledProxy")
      val proxy = new ControllerApi(controllerApiResource :: Nil).startProxy().await(99.s)
      try {
        val whenProcessed = proxy.eventBus.when[OrderProcessed].runToFuture
        val whenFinished = proxy.eventBus.when[OrderFinished.type].runToFuture
        controller.addOrder(FreshOrder(OrderId("🔺"), workflow.id.path)).runAsyncAndForget

        val processed = whenProcessed.await(99.s)
        assert(processed.stampedEvent.value.event == OrderProcessed(Outcome.succeeded))
        assert(processed.state.idToOrder(OrderId("🔺")).state == Order.Processed)

        whenFinished await 99.s  // Await order termination before shutting down the JS7
      } finally proxy.stop await 99.s
    }
  }

  "JControllerProxy with Flux" in {
    directoryProvider.runAgents() { _ =>
      val port = findFreeTcpPort()
      val controller = directoryProvider.startController(httpPort = Some(port)).await(99.s)
      try {
        val admissions = List(JAdmission.of(s"http://127.0.0.1:$port", primaryCredentials)).asJava
        val tester = new JControllerFluxTester(admissions, JHttpsConfig.empty)
        tester.test()
        tester.close()
      } finally {
        controller.terminate() await 99.s
        controller.close()
      }
    }
  }

  "updateRepo" in {
    val versionId = VersionId("MY-VERSION")
    val workflow = WorkflowParser.parse(s"""
      define workflow {
        execute executable="/path-to-my-script", agent="/AGENT",
          arguments = { "A": "${"A" * 700}" };
      }""").orThrow.withoutSource
    val n = calculateNumberOf[FileBased](workflow.withId(WorkflowPath("/WORKFLOW-XXXXX") ~ versionId))
    directoryProvider.runController() { controller =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(controller.localUri, Some(userAndPassword), name = "JournaledProxy")
      val api = new ControllerApi(controllerApiResource :: Nil)
      val workflowPaths = (1 to n).map(i => WorkflowPath(s"/WORKFLOW-$i"))
      val sw = new Stopwatch
      val operations = Observable.fromIterable(workflowPaths)
        .mapParallelUnordered(sys.runtime.availableProcessors)(path => Task(
          UpdateRepoOperation.AddOrReplace(
            sign(workflow.copy(id = path ~ versionId)))))
          .toListL await 99.s
      logger.info(sw.itemsPerSecondString(n, "signatures"))
      val logLine = measureTimeOfSingleRun(n, "workflows") {
        api.updateRepo(versionId, Observable.fromIterable(operations))
          .await(999.s).orThrow
      }
      logger.info(logLine.toString)
      // TODO Await Event
      val proxy = api.startProxy().await(99.s)
      try {
        waitForCondition(30.s, 50.ms)(proxy.currentState.repo.currentTyped[Workflow].sizeIs == n + 1)
        assert(proxy.currentState.repo.currentTyped[Workflow].keys.toVector.sorted == (
          workflowPaths :+ JournaledProxyTest.workflow.path).sorted)
      } finally proxy.stop await 99.s
    }
  }

  "addOrders" in {
    val bigOrder = FreshOrder(OrderId("ORDER"), JournaledProxyTest.workflow.path, Some(Timestamp("2100-01-01T00:00:00Z")),
      arguments = Map("A" -> "*" * 800))
    val n = calculateNumberOf(Stamped(0L, bigOrder.toOrderAdded(workflow.id.versionId): KeyedEvent[OrderEvent]))
    directoryProvider.runController() { controller =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(controller.localUri, Some(userAndPassword), name = "JournaledProxy")
      val api = new ControllerApi(controllerApiResource :: Nil)
      val orderIds = (1 to n).map(i => OrderId(s"ORDER-$i"))
      val logLine = measureTimeOfSingleRun(n, "orders") {
        api.addOrders(
          Observable.fromIterable(orderIds)
            .map(orderId => bigOrder.copy(id = orderId))
        ).await(199.s).orThrow
      }
      logger.info(logLine.toString)
      val proxy = api.startProxy().await(99.s)
      try {
        waitForCondition(30.s, 50.ms)(proxy.currentState.idToOrder.sizeIs == n)
        assert(proxy.currentState.idToOrder.keys.toVector.sorted == orderIds.sorted)
      } finally proxy.stop await 99.s
    }
  }

  private def calculateNumberOf[A: Encoder: TypeTag](sample: A): Int =
    if (sys.props.contains("test.speed") && sys.runtime.maxMemory >= 16_000_000_000L) {
      val sampleSize = sample.asJson.compactPrint.length
      val n = 1000_000
      logger.info(s"$n× ${implicitly[TypeTag[A]].tpe} à $sampleSize bytes = ${n * sampleSize} bytes")
      logger.info(sample.asJson.compactPrint)
      n
    } else
      1000

  "tornOlder" in {
    pending
  }
}

object JournaledProxyTest
{
  private val logger = Logger(getClass)
  private[proxy] val agentRefPath = AgentRefPath("/AGENT")
  private[proxy] val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "INITIAL",
    s"""
      define workflow {
        execute executable="/test.cmd", agent="AGENT", taskLimit=10;
      }"""
  ).orThrow

  private[proxy] val userAndPassword = UserAndPassword(UserId("Proxy") -> SecretString("PROXYS-PASSWORD-FOR-PRIMARY"))
  private[proxy] val primaryCredentials = JCredentials.JUserAndPassword(userAndPassword)
}
