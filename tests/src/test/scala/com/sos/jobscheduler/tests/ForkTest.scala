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
import com.sos.jobscheduler.common.system.OperatingSystem.LineEnd
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderMovedToAgent, OrderMovedToMaster, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, NodeKey, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.ForkTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class ForkTest extends FreeSpec {

  "test" in {
    val eventCollector = new TestEventCollector

    autoClosing(new DirectoryProvider(List(AgentPath("/AGENT")))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        import directoryProvider.directory

        (directoryProvider.master.live / "WORKFLOW.workflow.json").contentString = TestWorkflow.asJson.toPrettyString
        directoryProvider.agents(0).job(TestJobPath).xml = jobXml(100.ms)

        val agentConfs = directoryProvider.agents map (_.conf)

        runAgents(agentConfs) { _ ⇒
          RunningMaster.runForTest(directory) { master ⇒
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.executeCommand(MasterCommand.AddOrderIfNew(TestOrder)) await 99.s
            val EventSeq.NonEmpty(_) = eventCollector.when[OrderFinished.type](
              EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), _.key.string startsWith TestOrder.id.string) await 99.s
            val EventSeq.NonEmpty(eventSeq) = eventCollector.byPredicate[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.s), _ ⇒ true) await 99.s
            val keyedEvents = eventSeq.map(_.value).toVector
            assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
            for (orderId ← Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
              assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
            }
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
}

object ForkTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestJobPath = JobPath("/JOB")

  private val A  = JobNode(NodeId("A" ), TestAgentPath, TestJobPath)
  private val Bx = JobNode(NodeId("Bx"), TestAgentPath, TestJobPath)
  private val By = JobNode(NodeId("By"), TestAgentPath, TestJobPath)
  private val Cx = JobNode(NodeId("Cx"), TestAgentPath, TestJobPath)
  private val Cy = JobNode(NodeId("Cy"), TestAgentPath, TestJobPath)
  private val D  = JobNode(NodeId("D" ), TestAgentPath, TestJobPath)
  private val END = EndNode(NodeId("END"))

  private val fork = Transition(Vector(A), Vector(Bx, By), ForkTransition)
  private val bx = Transition(Bx, Cx)
  private val by = Transition(By, Cy)
  private val join = Transition.join(fromForked = A, fromProcessed = Vector(Cx, Cy), to = Vector(D), JoinTransition)
  private val d = Transition(D, END)

  private val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, Vector(fork, bx, by, join, d))
  private val TestOrder = Order(OrderId("🍎"), TestWorkflow.inputNodeKey, Order.StartNow, Some(Order.AttachedTo.Agent(TestAgentPath)),
    Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId("🍎/Bx")
  private val YOrderId = OrderId("🍎/By")
  private val ExpectedEvents = Vector(
    KeyedEvent(OrderAdded(NodeKey(TestWorkflow.path, A.id), Order.StartNow, Payload(Map("VARIABLE" → "VALUE"))))(TestOrder.id),
    KeyedEvent(OrderMovedToAgent(TestAgentPath))(TestOrder.id),
    KeyedEvent(OrderProcessingStarted)(TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(TestOrder.id),
    KeyedEvent(OrderForked(Vector(
      OrderForked.Child(XOrderId, Bx.id, Payload(Map("VARIABLE" → "VALUE"))),    OrderForked.Child(YOrderId, By.id, Payload(Map("VARIABLE" → "VALUE")))))) (TestOrder.id),
      KeyedEvent(OrderProcessingStarted)(XOrderId),                              KeyedEvent(OrderProcessingStarted)(YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(XOrderId),        KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(YOrderId),
      KeyedEvent(OrderMoved(Cx.id))(XOrderId),                                   KeyedEvent(OrderMoved(Cy.id))(YOrderId),
      KeyedEvent(OrderProcessingStarted)(XOrderId),                              KeyedEvent(OrderProcessingStarted)(YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(XOrderId),        KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(YOrderId),
    KeyedEvent(OrderJoined(D.id, MapDiff.empty, Outcome.Good(true)))(TestOrder.id),
    KeyedEvent(OrderProcessingStarted)(TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))(TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))(TestOrder.id),
    KeyedEvent(OrderDetachable)(TestOrder.id),
    KeyedEvent(OrderMovedToMaster)(TestOrder.id),
    KeyedEvent(OrderMoved(END.id))(TestOrder.id),
    KeyedEvent(OrderFinished)(TestOrder.id))

  private val logger = Logger(getClass)
}
