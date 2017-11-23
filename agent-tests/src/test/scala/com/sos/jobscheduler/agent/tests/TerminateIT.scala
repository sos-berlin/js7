package com.sos.jobscheduler.agent.tests

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector}
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, Login, RegisterAsMaster, Terminate}
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.agent.tests.TerminateIT._
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.soslicense.LicenseKeyString
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.ActorEventCollector
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class TerminateIT extends FreeSpec with BeforeAndAfterAll  {

  "Terminate" in {
    withCloser { implicit closer ⇒
      implicit val actorSystem = ActorSystem("TerminateIT")
      closer onClose actorSystem.terminate()
      provideAgent { (client, agent) ⇒
        val eventCollector = newEventCollector(agent.injector)
        val lastEventId = eventCollector.lastEventId

        val orderIds = for (i ← 0 until 3) yield OrderId(s"TEST-ORDER-$i")
        (for (orderId ← orderIds) yield
          client.executeCommand(AttachOrder(
            Order(
              orderId,
              NodeKey(AJobnet.path, NodeId("100")),
              Order.Ready,
              Map("a" → "A")),
            AJobnet))
        ) await 99.s

        val whenStepEnded: Future[Seq[OrderEvent.OrderProcessed]] =
          Future.sequence(
            for (orderId ← orderIds) yield
              eventCollector.whenKeyedEvent[OrderEvent.OrderProcessed](EventRequest.singleClass(after = lastEventId, 90.s), orderId))
        sleep(2.s)
        assert(!whenStepEnded.isCompleted)

        client.executeCommand(Terminate(sigkillProcessesAfter = Some(0.s))) await 99.s
        val stepEnded = whenStepEnded await 99.s
        assert(stepEnded forall { e ⇒ !e.outcome.asInstanceOf[Order.Good].returnValue })
        agent.terminated await 99.s
      }
    }
  }
}

object TerminateIT {
  private val TestAgentId = AgentPath("/TEST-AGENT")
  private val AJobPath = JobPath("/test")
  private val AJobnet = Jobnet(
    JobnetPath("/A"),
    NodeId("100"),
    List(
      Jobnet.JobNode(NodeId("100"), TestAgentId, AJobPath, onSuccess = NodeId("END"), onFailure = NodeId("END")),
      Jobnet.EndNode(NodeId("END"))))
  private val AScript =
    if (isWindows) """
      |@echo off
      |ping -n 11 127.0.0.1 >nul
      |""".stripMargin
    else """
      |sleep 10
      |""".stripMargin

  private def provideAgent(body: (AgentClient, RunningAgent) ⇒ Unit)(implicit actorSystem: ActorSystem, closer: Closer): Unit = {
    TestAgentDirectoryProvider.provideAgentDirectory { agentDirectory ⇒
      (agentDirectory / "config" / "live" / "test.job.xml").xml =
        <job tasks="10">
          <script language="shell">{AScript}</script>
        </job>
      val agent = RunningAgent(AgentConfiguration.forTest(configAndData = Some(agentDirectory))) map { _.closeWithCloser } await 10.s
      implicit val actorRefFactory: ActorRefFactory = newActorSystem("TerminateIT")(closer)
      val client = AgentClient(
        agentUri = agent.localUri.toString,
        licenseKeys = List(LicenseKeyString("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")))
      client.executeCommand(Login) await 99.s
      client.executeCommand(RegisterAsMaster) await 99.s
      body(client, agent)
    }
  }

  private def newEventCollector(injector: Injector) =
    injector.createChildInjector(new AbstractModule {
      def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000, timeoutLimit = 99.s)
    }).instance[ActorEventCollector]
}
