package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Goto, IfNonZeroReturnCodeGoto, Job, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.{Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.order.LegacyJobchainXmlParser
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.LegacyJobchainTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
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
        directoryProvider.master.writeJson(TestWorkflow.withoutVersion)
        for (a ← directoryProvider.agents) a.file(Test0JobPath, SourceType.Xml).xml = jobXml(ReturnCode(0))
        for (a ← directoryProvider.agents) a.file(Test1JobPath, SourceType.Xml).xml = jobXml(ReturnCode(1))

        directoryProvider.runAgents() { _ ⇒
          directoryProvider.runMaster() { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.addOrderBlocking(TestOrder)
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
    .orThrow.copy(id = WorkflowPath("/WORKFLOW") % "(initial)")
  private val ExpectedWorkflow = Workflow.of(TestWorkflow.id,
    "A" @: /*0*/ Job(JobPath("/JOB-0"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
           /*1*/ IfNonZeroReturnCodeGoto("FAILURE"),
           /*2*/ Goto("B"),
    "X" @: /*3*/ ExplicitEnd,
    "B" @: /*4*/ Job(JobPath("/JOB-1"), AgentPath("/AGENT"), ReturnCodeMeaning.NoFailure),
           /*5*/ IfNonZeroReturnCodeGoto("FAILURE"),
    "END" @: /*6*/ ExplicitEnd,
    "FAILURE" @: /*7*/ ExplicitEnd)

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path)
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id),
    TestOrder.id <-: OrderTransferredToAgent(TestAgentPath % "(initial)"),
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
