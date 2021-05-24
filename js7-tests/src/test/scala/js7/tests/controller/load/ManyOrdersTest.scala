package js7.tests.controller.load

import js7.base.configutils.Configs._
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.ByteUnits.toKBGB
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.event.{EventId, EventRequest}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.StringValue
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.load.ManyOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class ManyOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow)

  override protected val controllerConfig = config"""
    js7.journal.remove-obsolete-files = false"""

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  private lazy val (n, orderSize) = sys.props.get("test.speed").map(_.split(" +")) match {
    case None => (defaultN, defaultSize)

    case Some(Array(nString)) =>
      val (a, b) = nString.span(_ != '*')
      (a.toInt, b.drop(1).toInt)

    case _ => sys.error("Invalid number of arguments in property test.speed")
  }

  override def afterAll() = {
    controller.terminate() await longTimeout
    super.afterAll()
  }

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
  private val defaultSize = 4_000_000
  private val longTimeout = 999.s
  private val agentPath = AgentPath("AGENT")
  private val workflow = WorkflowParser.parse(
    WorkflowPath("WORKFLOW") ~ "1", s"""
      define workflow {
        execute agent="AGENT", internalJobClass="js7.tests.jobs.EmptyJob", taskLimit=32;
      }"""
  ).orThrow
}
