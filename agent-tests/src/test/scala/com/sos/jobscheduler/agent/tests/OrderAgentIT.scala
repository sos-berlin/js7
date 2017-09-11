package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachJobnet, AttachOrder, Compound, DetachOrder, Login, RegisterAsMaster}
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider.provideAgentDirectory
import com.sos.jobscheduler.agent.tests.OrderAgentIT._
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.OrderReady
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.mutable
/**
  * @author Joacim Zschimmer
  */
final class OrderAgentIT extends FreeSpec {

  "AgentCommand AttachOrder" in {
    provideAgentDirectory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir / "a.job.xml").xml = AJobXml
      (jobDir / "b.job.xml").xml = BJobXml
      val agentConf = AgentConfiguration.forTest(Some(directory))
      RunningAgent.run(agentConf, timeout = Some(99.s)) { agent ⇒
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri.toString).closeWithCloser

          agentClient.executeCommand(RegisterAsMaster) await 99.s shouldEqual AgentCommand.Accepted  // Without Login, this registers all anonymous clients
          agentClient.executeCommand(AttachJobnet(TestJobnet)) await 99.s shouldEqual AgentCommand.Accepted

          val order = Order(OrderId("TEST-ORDER"), NodeKey(TestJobnet.path, StartNodeId), Order.Waiting, Map("x" → "X"))
          agentClient.executeCommand(AttachOrder(order)) await 99.s shouldEqual AgentCommand.Accepted

          while (
            agentClient.mastersEvents(EventRequest.singleClass[OrderEvent](after = EventId.BeforeFirst, timeout = 10.s)) await 99.s match {
              case EventSeq.NonEmpty(stampeds) if stampeds map { _.value } contains KeyedEvent(OrderReady)(order.id) ⇒
                false
              case _ ⇒
                true
            }
          ) {}

          val processedOrder = agentClient.order(order.id) await 99.s
          assert(processedOrder == toExpectedOrder(order))
          agentClient.executeCommand(DetachOrder(order.id)) await 99.s shouldEqual AgentCommand.Accepted
          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))
          agentClient.executeCommand(AgentCommand.Terminate()) await 99.s
        }
      }
    }
  }

  for (testSpeed ← sys.props.get("test.speed")) s"Speed test $testSpeed orders × ${TestJobnet.jobNodeCount} jobs" in {
    val n = testSpeed.toInt
    provideAgentDirectory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir / "a.job.xml").xml = AJobXml
      (jobDir / "b.job.xml").xml = BJobXml
      val agentConf = AgentConfiguration.forTest(Some(directory))
      val timeout = 1.h
      RunningAgent.run(agentConf, timeout = Some(timeout)) { agent ⇒
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri.toString).closeWithCloser
          agentClient.executeCommand(Login) await 99.s
          agentClient.executeCommand(RegisterAsMaster) await 99.s

          val orders = for (i ← 1 to n) yield
            Order(OrderId(s"TEST-ORDER-$i"), NodeKey(TestJobnet.path, StartNodeId), Order.Waiting, Map("x" → "X"))

          val stopwatch = new Stopwatch
          agentClient.executeCommand(Compound(AttachJobnet(TestJobnet) +: (orders map AttachOrder.apply))) await 99.s

          val awaitedOrderIds = (orders map { _.id }).toSet
          val ready = mutable.Set[OrderId]()
          while (
            agentClient.mastersEvents(EventRequest.singleClass[OrderEvent](after = EventId.BeforeFirst, timeout = timeout)) await 99.s match {
              case EventSeq.NonEmpty(stampeds) ⇒
                ready ++= stampeds map { _.value } collect { case KeyedEvent(orderId: OrderId, OrderReady) ⇒ orderId }
                ready != awaitedOrderIds
              case _ ⇒
                true
            }
          ) {}
          agentClient.executeCommand(Compound(orders map { o ⇒ DetachOrder(o.id) })) await 99.s
          info(stopwatch.itemsPerSecondString(n, "orders"))

          agentClient.executeCommand(AgentCommand.Terminate()) await 99.s
        }
      }
    }
  }
}

private object OrderAgentIT {
  private val TestAgentId = AgentPath("/TEST-AGENT")
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private val AJobXml =
    <job tasks={sys.runtime.availableProcessors.toString}>
      <params>
        <param name="var1" value="AAA"/>
      </params>
      <script language="shell">{TestScript}</script>
    </job>

  private val BJobXml =
    <job tasks={sys.runtime.availableProcessors.toString}>
      <params>
        <param name="var1" value="BBB"/>
      </params>
      <script language="shell">{TestScript}</script>
    </job>

  private val AJobPath = JobPath("/a")
  private val BJobPath = JobPath("/b")
  private val StartNodeId = NodeId("0")
  private val EndNodeId = NodeId("END")
  private val FailedNodeId = NodeId("FAILED")
  private val TestJobnet = Jobnet(
    JobnetPath("/TEST"),
    StartNodeId,
    List(
      Jobnet.JobNode(StartNodeId, TestAgentId, AJobPath, onSuccess = NodeId("100"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("100"), TestAgentId, BJobPath, onSuccess = NodeId("200"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("200"), TestAgentId, BJobPath, onSuccess = NodeId("300"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("300"), TestAgentId, BJobPath, onSuccess = NodeId("400"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("400"), TestAgentId, BJobPath, onSuccess = NodeId("500"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("500"), TestAgentId, BJobPath, onSuccess = NodeId("600"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("600"), TestAgentId, BJobPath, onSuccess = NodeId("700"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("700"), TestAgentId, BJobPath, onSuccess = NodeId("800"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("800"), TestAgentId, BJobPath, onSuccess = NodeId("900"), onFailure = FailedNodeId),
      Jobnet.JobNode(NodeId("900"), TestAgentId, BJobPath, onSuccess = EndNodeId, onFailure = FailedNodeId),
      Jobnet.EndNode(EndNodeId),
      Jobnet.EndNode(FailedNodeId)))

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      nodeKey = order.nodeKey.copy(nodeId = EndNodeId),
      state = Order.Ready,
      outcome = Order.Good(true),
      variables = Map("x" → "X", "result" → "TEST-RESULT-BBB"))
}
