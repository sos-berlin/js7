package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import cats.syntax.either.catsSyntaxEither
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
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Instruction.IfReturnCode
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.workflow.notation.WorkflowParser
import com.sos.jobscheduler.tests.IfReturnCodeTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

final class IfReturnCodeTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        import directoryProvider.directory

        directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestNamedWorkflow.workflow.asJson.toPrettyString
        for (a ← directoryProvider.agents) a.job(TestJobPath).xml = <job tasks="3"><script language="shell">exit</script></job>

        runAgents(directoryProvider.agents map (_.conf)) { _ ⇒
          RunningMaster.runForTest(directory) { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.executeCommand(MasterCommand.AddOrderIfNew(TestOrder)) await 99.s
            val EventSeq.NonEmpty(_) = eventCollector.when[OrderFinished.type](
              EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), _.key.string startsWith TestOrder.id.string) await 99.s
            val eventSeq = eventCollector.byPredicate[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.s), _ ⇒ true) await 99.s
            checkEventSeq(eventSeq)
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
      |}
      |job "JOB" on "AGENT";    // #2
    """.stripMargin
  private val TestNamedWorkflow = Workflow.Named(
    WorkflowPath("/WORKFLOW"),
    WorkflowParser.parse(script) valueOr sys.error)
  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow)

  val ExpectedEvents = Vector(
    OrderAdded(TestNamedWorkflow.path, Order.StartNow),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(1, 0, 0)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  private val logger = Logger(getClass)
}
