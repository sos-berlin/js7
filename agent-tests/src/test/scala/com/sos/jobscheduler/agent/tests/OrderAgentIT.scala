package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.{AddJobnet, AddOrder, DetachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider.provideAgent2Directory
import com.sos.jobscheduler.agent.tests.OrderAgentIT._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.OrderReady
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentIT extends FreeSpec {

  "Command AddOrder" in {
    provideAgent2Directory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir / "a.job.xml").xml = AJobXml
      (jobDir / "b.job.xml").xml = BJobXml
      val agentConf = AgentConfiguration.forTest(Some(directory))
        //config = ConfigFactory.parseMap(Map(
            //"akka.persistence.journal.plugin" → "dummy-journal",
            //"dummy-journal.class" → classOf[org.dmonix.akka.persistence.JournalPlugin].getName,
            //"dummy-journal.plugin-dispatcher" → "akka.actor.default-dispatcher")))
            //"akka.persistence.journal.plugin" → "akka.persistence.journal.leveldb",
            //"akka.persistence.journal.leveldb.dir" → s"$persistenceDir/persistence",
            //"akka.persistence.snapshot-store.plugin" → "akka.persistence.snapshot-store.local",
            //"akka.persistence.snapshot-store.local.dir" → s"$persistenceDir/snapshots")))
      autoClosing(new Agent(agentConf)) { agent ⇒
        agent.start() await 5.s
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName) withCloser { _.terminate() await 99.s }
          val agentClient = AgentClient(agent.localUri.toString)

          agentClient.executeCommand(RegisterAsMaster) await 99.s shouldEqual EmptyResponse  // Without Login, this registers all anonymous clients
          agentClient.executeCommand(AddJobnet(TestJobnet)) await 99.s shouldEqual EmptyResponse

          val order = Order(
            OrderId("TEST-ORDER"),
            NodeKey(TestJobnet.path, ANodeId),
            Order.Waiting,
            Map("x" → "X"))
          agentClient.executeCommand(AddOrder(order)) await 99.s shouldEqual EmptyResponse

          waitForCondition(10.s, 100.ms) {
            agentClient.mastersEvents(EventRequest.singleClass[OrderEvent](after = EventId.BeforeFirst, timeout = 10.s)) await 99.s match {
              case EventSeq.NonEmpty(stampeds) if stampeds map { _.value } contains KeyedEvent(OrderReady)(order.id) ⇒
                true
              case _ ⇒
                false
            }
          }

          val processedOrder = agentClient.order(order.id) await 99.s
          assert(processedOrder == order.copy(
            nodeKey = order.nodeKey.copy(nodeId = EndNodeId),
            state = Order.Ready,
            outcome = Order.Good(true),
            variables = Map("x" → "X", "result" → "TEST-RESULT-BBB")))

          agentClient.executeCommand(DetachOrder(order.id)) await 99.s shouldEqual EmptyResponse
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
    <job>
      <params>
        <param name="var1" value="AAA"/>
      </params>
      <script language="shell">{TestScript}</script>
    </job>

  private val BJobXml =
    <job>
      <params>
        <param name="var1" value="BBB"/>
      </params>
      <script language="shell">{TestScript}</script>
    </job>

  private val AJobPath = JobPath("/a")
  private val BJobPath = JobPath("/b")
  private val ANodeId = NodeId("AAA")
  private val BNodeId = NodeId("BBB")
  private val EndNodeId = NodeId("END")
  private val FailedNodeId = NodeId("FAILED")
  private val TestJobnet = Jobnet(
    JobnetPath("/TEST"),
    ANodeId,
    List(
      Jobnet.JobNode(ANodeId, TestAgentId, AJobPath, onSuccess = BNodeId, onFailure = FailedNodeId),
      Jobnet.JobNode(BNodeId, TestAgentId, BJobPath, onSuccess = EndNodeId, onFailure = FailedNodeId),
      Jobnet.EndNode(EndNodeId),
      Jobnet.EndNode(FailedNodeId)))
}
