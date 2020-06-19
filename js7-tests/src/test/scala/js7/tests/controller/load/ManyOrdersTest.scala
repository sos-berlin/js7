package js7.tests.controller.load

import com.typesafe.config.ConfigFactory
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.{Stopwatch, Timestamp}
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.data.agent.AgentRefPath
import js7.data.event.{EventId, EventRequest}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.tests.controller.load.ManyOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import org.scalatest.freespec.AnyFreeSpec

final class ManyOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil
  override protected val controllerConfig = ConfigFactory.parseString("""
    js7.webserver.auth.public = on
    js7.journal.remove-obsolete-files = false""")

  private lazy val n = sys.props.get("ManyOrdersTest").map(_.toInt) getOrElse 1000

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(ExecutablePath(s"/TEST$sh"), script(10.ms))
    }
    super.beforeAll()
  }

  override def afterAll() = {
    controller.terminate() await longTimeout
  }

  s"Add $n orders and make a snapshot" in {
    val t = new Stopwatch
    val orderId = for (i <- 1 to n) yield OrderId(s"ORDER-$i")
    addOrders(orderId)
    if (n >= 10000) println(t.itemsPerSecondString(n, "orders added"))
    controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s)
    if (n >= 10000) println(t.itemsPerSecondString(n, "orders written to snapshot"))
    //? waitUntilAllOrdersFinished(t)
  }

  private def addOrders(orderId: Seq[OrderId]): Unit =
    orderId.grouped(1000)
      .map(_.map(FreshOrder(_, workflow.path, Some(Timestamp("3000-01-01T00:00:00Z")))))
      .foreach { orders =>
        controller.httpApi.addOrders(orders).await(99.s)
      }

  private def waitUntilAllOrdersFinished(stopwatch: Stopwatch): Unit = {
    val finishedCount = AtomicInt(0)
    controller.eventWatch
      .observe(EventRequest.singleClass[OrderFinished](after = EventId.BeforeFirst, timeout = Some(99.s)))
      .takeWhile { _ =>
        finishedCount += 1
        val m = finishedCount.get
        if (n >= 10000 && m % 1000 == 0) println(stopwatch.itemsPerSecondString(m, s"orders finished"))
        finishedCount.get < n
      }
      .completedL
      .runToFuture.await(longTimeout)
  }
}

object ManyOrdersTest
{
  private val longTimeout = 1.h
  private val agentRefPath = AgentRefPath("/AGENT")
  private val   workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "1",s"""
      define workflow {
        execute executable="/TEST$sh", agent="/AGENT", taskLimit=100;
        execute executable="/TEST$sh", agent="/AGENT", taskLimit=100;
        execute executable="/TEST$sh", agent="/AGENT", taskLimit=100;
        execute executable="/TEST$sh", agent="/AGENT", taskLimit=100;
        execute executable="/TEST$sh", agent="/AGENT", taskLimit=100;
      }"""
  ).orThrow
}
