package com.sos.jobscheduler.tests.master.load

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Stopwatch
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.tests.master.load.ManyOrdersTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.master.data.MasterCommand.TakeSnapshot
import monix.execution.atomic.AtomicInt
import org.scalatest.freespec.AnyFreeSpec

final class ManyOrdersTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil
  override protected val masterConfig = ConfigFactory.parseString("""
    jobscheduler.webserver.auth.public = on
    jobscheduler.journal.remove-obsolete-files = false""")

  private lazy val n = sys.props.get("ManyOrdersTest").map(_.toInt) getOrElse 1000

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(ExecutablePath(s"/TEST$sh"), script(10.ms))
    }
    super.beforeAll()
  }

  override def afterAll() = {
    master.terminate() await longTimeout
  }

  s"Add $n orders and make a snapshot" in {
    val t = new Stopwatch
    val orderId = for (i <- 1 to n) yield OrderId(s"ORDER-$i")
    addOrders(orderId)
    if (n >= 10000) println(t.itemsPerSecondString(n, "orders added"))
    master.executeCommandAsSystemUser(TakeSnapshot).await(99.s)
    if (n >= 10000) println(t.itemsPerSecondString(n, "orders written to snapshot"))
    //? waitUntilAllOrdersFinished(t)
  }

  private def addOrders(orderId: Seq[OrderId]): Unit =
    orderId.grouped(1000)
      .map(_.map(FreshOrder(_, workflow.path, Some(Timestamp("3000-01-01T00:00:00Z")))))
      .foreach { orders =>
        master.httpApi.addOrders(orders).await(99.s)
      }

  private def waitUntilAllOrdersFinished(stopwatch: Stopwatch): Unit = {
    val finishedCount = AtomicInt(0)
    master.eventWatch
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
