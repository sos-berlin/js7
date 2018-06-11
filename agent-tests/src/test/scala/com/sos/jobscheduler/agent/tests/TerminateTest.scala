package com.sos.jobscheduler.agent.tests

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsMaster, Terminate}
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.agent.tests.TerminateTest._
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.ActorEventCollector
import com.sos.jobscheduler.data.event.{EventId, EventRequest}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import io.circe.syntax.EncoderOps
import javax.inject.Singleton
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class TerminateTest extends FreeSpec with BeforeAndAfterAll  {

  "Terminate" in {
    withCloser { implicit closer ⇒
      implicit val actorSystem = newActorSystem("TerminateTest")
      closer onClose actorSystem.terminate()
      provideAgent { (client, agent) ⇒
        val eventCollector = newEventCollector(agent.injector)

        val orderIds = for (i ← 0 until 3) yield OrderId(s"TEST-ORDER-$i")
        (for (orderId ← orderIds) yield
          client.executeCommand(AttachOrder(
            Order(
              orderId,
              SimpleTestWorkflow.id,
              Order.Ready,
              payload = Payload(Map("a" → "A"))),
            TestAgentPath % "(initial)",
            SimpleTestWorkflow))
        ) await 99.s

        val whenStepEnded: Future[Seq[OrderEvent.OrderProcessed]] =
          Future.sequence(
            for (orderId ← orderIds) yield
              eventCollector.whenKeyedEvent[OrderEvent.OrderProcessed](EventRequest.singleClass(after = EventId.BeforeFirst, 90.seconds), orderId)
                .runAsync: Future[OrderEvent.OrderProcessed])
        sleep(2.s)
        assert(!whenStepEnded.isCompleted)

        client.executeCommand(Terminate(sigkillProcessesAfter = Some(0.seconds))) await 99.s
        val stepEnded = whenStepEnded await 99.s
        assert(stepEnded forall { _.outcome.asInstanceOf[Outcome.Undisrupted].isFailed })
        agent.terminated await 99.s
      }
    }
  }
}

object TerminateTest {
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
      (agentDirectory / "config" / "private" / "private.conf").contentString = """ jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD" """
      (agentDirectory / "config" / "live" resolve AJob.jobPath.toFile(SourceType.Json)).contentString =
        JobConfiguration(
          JobPath.NoId,
          JobScript(AScript),
          Map.empty,
          taskLimit = 10
        ).asJson.toPrettyString
      val agent = RunningAgent.startForTest(AgentConfiguration.forTest(configAndData = Some(agentDirectory))) map { _.closeWithCloser } await 10.s
      implicit val actorRefFactory: ActorRefFactory = newActorSystem("TerminateTest")(closer)
      val client = AgentClient(
        agentUri = agent.localUri.toString)
        //licenseKeys = List(LicenseKeyString("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")))
      client.login(Some(UserId("TEST-USER") → SecretString("TEST-PASSWORD"))) await 99.s
      client.executeCommand(RegisterAsMaster) await 99.s
      body(client, agent)
      agent.close()
    }
  }

  private def newEventCollector(injector: Injector): EventCollector =
    injector.createChildInjector(new AbstractModule {
      override def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000, timeoutLimit = 99.s)

      @Provides @Singleton
      def eventCollector(factory: ActorEventCollector.Factory): EventCollector =
        factory.apply()
    }).instance[EventCollector]
}
