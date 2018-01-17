package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderMovedToAgent, OrderMovedToMaster, OrderProcessed, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.LegacyJobchainXmlParser
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.workflow.Workflows.ExecutableWorkflowScript
import com.sos.jobscheduler.tests.LegacyJobchainTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

final class LegacyJobchainTest extends FreeSpec {

  "Workflow" in {
    assert(TestWorkflow.copy(source = None) == ExpectedWorkflow)
  }

  "reduceForAgent" in {
    assert(TestWorkflow.reduceForAgent(TestAgentPath) == ExpectedWorkflow)  // Single Agent, nothing to reduce
  }

  "Run workflow" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        import directoryProvider.directory

        directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestWorkflow.asJson.toPrettyString
        for (a ← directoryProvider.agents) a.job(Test0JobPath).xml = jobXml(ReturnCode(0))
        for (a ← directoryProvider.agents) a.job(Test1JobPath).xml = jobXml(ReturnCode(1))

        runAgents(directoryProvider.agents map (_.conf)) { _ ⇒
          RunningMaster.runForTest(directory) { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.executeCommand(MasterCommand.AddOrderIfNew(TestOrder)) await 99.s
            val EventSeq.NonEmpty(_) = eventCollector.when[OrderFinished.type](
              EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), _.key == TestOrder.id) await 99.s
            checkEventSeq(
              eventCollector.byPredicate[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.s), _ ⇒ true) await 99.s)
          }
        }
      }
    }
  }

  private def runAgents(confs: Seq[AgentConfiguration])(body: Seq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(confs map startAgent await 10.s) { agents ⇒
      body(agents)
    }

  private def startAgent(conf: AgentConfiguration): Future[RunningAgent] = {
    val whenAgent = RunningAgent(conf)
    for (agent ← whenAgent; t ← agent.terminated.failed) logger.error(t.toStringWithCauses, t)
    whenAgent
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
           /*1*/ IfError("FAILURE"),
           /*2*/ Goto("B"),
    "X" @: /*3*/ ExplicitEnd,
    "B" @: /*4*/ Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-1"))),
           /*5*/ IfError("FAILURE"),
    "END" @: /*6*/ ExplicitEnd,
    "FAILURE" @: /*7*/ ExplicitEnd))

  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow)
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestNamedWorkflow.path, Order.StartNow, Payload.empty),
    TestOrder.id <-: OrderMovedToAgent(TestAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    TestOrder.id <-: OrderMoved(4),   // next_state="B"
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(false)),
    TestOrder.id <-: OrderMoved(7),   // error_state="FAILURE"
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderMovedToMaster,
    TestOrder.id <-: OrderFinished)

  private def jobXml(returnCode: ReturnCode) =
    <job>
      <script language="shell">{
        (if (isWindows) "@echo off\n" else "") +
          "exit " + returnCode.number + "\n"
        }</script>
    </job>
}
