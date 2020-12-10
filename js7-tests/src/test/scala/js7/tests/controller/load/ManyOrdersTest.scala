package js7.tests.controller.load

import js7.base.auth.Admission
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.common.configutils.Configs._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.ByteUnits.toKBGB
import js7.controller.client.AkkaHttpControllerApi.admissionToApiResource
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.data.agent.AgentId
import js7.data.event.{EventId, EventRequest}
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.StringValue
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.proxy.ControllerApi
import js7.tests.controller.load.ManyOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class ManyOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val inventoryItems = workflow :: Nil
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false"""

  private lazy val controllerApi = new ControllerApi(Seq(
    admissionToApiResource(Admission(controller.localUri, None))(controller.actorSystem)))
  private lazy val n = sys.props.get("test.speed").fold(defaultN)(_.toInt)

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(RelativeExecutablePath(s"TEST$sh"), script(10.ms))
    }
    super.beforeAll()
  }

  override def afterAll() =
    controller.terminate() await longTimeout

  s"Add $n orders à ${toKBGB(orderSize)} and make a snapshot" in {
    val t = new Stopwatch
    addOrders()
    if (n > defaultN) println(t.itemsPerSecondString(n, "orders added"))
    controller.executeCommandAsSystemUser(TakeSnapshot) await longTimeout
    if (n > defaultN) println(t.itemsPerSecondString(n, "orders written to snapshot"))
    waitUntilAllOrdersFinished(t)
    if (n > defaultN) println(t.itemsPerSecondString(n, "orders processed"))
  }

  private def addOrders(): Unit = {
    val payload = "BIG-"
    val order = FreshOrder(OrderId(s"ORDER"), workflow.path,
      arguments = Map("BIG" -> StringValue(payload * (orderSize / payload.length))))
    controllerApi
      .addOrders(Observable
        .fromIterable(1 to n)
        .map(i => order.copy(id = OrderId(s"ORDER-$i"))))
      .await(longTimeout).orThrow
  }

  def waitUntilAllOrdersFinished(stopwatch: Stopwatch): Unit =
    controller.eventWatch
      .observe(EventRequest.singleClass[OrderFinished](after = EventId.BeforeFirst, timeout = None))
      .scan(0)((i, _) => i + 1)
      .map { i =>
        if (n > 100 && i % 100 == 0) println(stopwatch.itemsPerSecondString(i, s"orders finished"))
        i
      }
      .dropWhile(_ < n)
      .headOptionL
      .await(longTimeout)
}

object ManyOrdersTest
{
  private val defaultN = 10
  private val orderSize = 4_000_000
  private val longTimeout = 999.s
  private val agentId = AgentId("AGENT")
  private val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "1", s"""
      define workflow {
        execute executable="TEST$sh", agent="AGENT", taskLimit=1000, v1Compatible=false;
      }"""
  ).orThrow
}
