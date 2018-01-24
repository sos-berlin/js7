package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Goto, IfFailedGoto, Job}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.order.LegacyJobchainXmlParser
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.tests.LegacyJobchainTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

final class LegacyJobchainTest extends FreeSpec {

  "Workflow" in {
    assert(TestWorkflow.withoutSource == ExpectedWorkflow)
  }

  "reduceForAgent" in {
    assert(TestWorkflow.reduceForAgent(TestAgentPath) == ExpectedWorkflow)  // Single Agent, nothing to reduce
  }

  "Run workflow" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestWorkflow.asJson.toPrettyString
        for (a ← directoryProvider.agents) a.job(Test0JobPath).xml = jobXml(ReturnCode(0))
        for (a ← directoryProvider.agents) a.job(Test1JobPath).xml = jobXml(ReturnCode(1))

        directoryProvider.runAgents { _ ⇒
          directoryProvider.runMaster { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.addOrder(TestOrder) await 99.s
            eventCollector.await[OrderFinished](_.key == TestOrder.id)
            checkEventSeq(eventCollector.all[OrderEvent])
          }
        }
      }
    }
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        assert(keyedEvents == ExpectedEvents)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object LegacyJobchainTest {

  private val logger = Logger(getClass)
  private val TestAgentPath = AgentPath("/AGENT")
  private val Test0JobPath = JobPath("/JOB-0")
  private val Test1JobPath = JobPath("/JOB-1")
  private val TestWorkflow = LegacyJobchainXmlParser.parseXml(
    FolderPath("/FOLDER"),
    <job_chain>
      <job_chain_node     state="A" agent="/AGENT" job="/JOB-0" next_state="B" error_state="FAILURE"/>
      <job_chain_node.end state="X"/>
      <job_chain_node     state="B" agent="/AGENT" job="/JOB-1" error_state="FAILURE"/>
      <job_chain_node     state="END"/>
      <job_chain_node.end state="FAILURE"/>
    </job_chain>.toString())
  private val TestNamedWorkflow = Workflow.Named(WorkflowPath("/WORKFLOW"), TestWorkflow)
  private val ExpectedWorkflow = Workflow(Vector(
    "A" @: /*0*/ Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-0"))),
           /*1*/ IfFailedGoto("FAILURE"),
           /*2*/ Goto("B"),
    "X" @: /*3*/ ExplicitEnd,
    "B" @: /*4*/ Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-1"))),
           /*5*/ IfFailedGoto("FAILURE"),
    "END" @: /*6*/ ExplicitEnd,
    "FAILURE" @: /*7*/ ExplicitEnd))

  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow)
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestNamedWorkflow.path, Order.StartNow, Payload.empty),
    TestOrder.id <-: OrderTransferredToAgent(TestAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),   // next_state="B"
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(1))),
    TestOrder.id <-: OrderMoved(Position(7)),   // error_state="FAILURE"
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderFinished)

  private def jobXml(returnCode: ReturnCode) =
    <job>
      <script language="shell">{
        (if (isWindows) "@echo off\n" else "") +
          "exit " + returnCode.number + "\n"
        }</script>
    </job>
}
