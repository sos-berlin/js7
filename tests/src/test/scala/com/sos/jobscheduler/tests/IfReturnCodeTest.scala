package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import cats.syntax.either.catsSyntaxEither
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{JobPath, Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.workflow.notation.WorkflowParser
import com.sos.jobscheduler.tests.IfReturnCodeTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

final class IfReturnCodeTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestNamedWorkflow.workflow.asJson.toPrettyString
      for (a ← directoryProvider.agents) a.job(TestJobPath).xml = <job tasks="3"><script language="shell">:</script></job>

      directoryProvider.run { (master, _) ⇒
        val eventCollector = new TestEventCollector
        eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
        master.addOrder(TestOrder) await 99.s
        eventCollector.await[OrderFinished](_.key == TestOrder.id)
        checkEventSeq(eventCollector.all[OrderEvent])
      }
    }
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val events = stampeds.map(_.value.event).toVector
        assert(events == ExpectedEvents)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object IfReturnCodeTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestJobPath = JobPath("/JOB")
  private val script = """
      |job "JOB" on "AGENT";    // #0
      |if (returnCode 0) {      // #1
      |  job "JOB" on "AGENT";  // #1/0/0
      |} else {
      |  job "JOB" on "AGENT";  // #1/1/0
      |};
      |job "JOB" on "AGENT";    // #2
    """.stripMargin
  private val TestNamedWorkflow = Workflow.Named(
    WorkflowPath("/WORKFLOW"),
    WorkflowParser.parse(script) valueOr sys.error)
  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow)

  private val ExpectedEvents = Vector(
    OrderAdded(TestNamedWorkflow.path, Order.StartNow),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    OrderMoved(Position(1, 0, 0)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)
}
