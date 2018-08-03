package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, Batch, DetachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider.{TestUserAndPassword, provideAgentDirectory}
import com.sos.jobscheduler.agent.tests.OrderAgentTest._
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetachable
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.typesafe.config.ConfigFactory
import io.circe.syntax.EncoderOps
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentTest extends FreeSpec {

  "AgentCommand AttachOrder" in {
    provideAgentDirectory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir resolve AJob.jobPath.toFile(SourceType.Json)).contentString = AJobConfiguration.asJson.toPrettyString
      (jobDir resolve BJob.jobPath.toFile(SourceType.Json)).contentString = BJobConfiguration.asJson.toPrettyString
      val agentConf = AgentConfiguration.forTest(directory)
      RunningAgent.run(agentConf, timeout = Some(99.s)) { agent ⇒
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri.toString).closeWithCloser
          intercept[AkkaHttpClient.HttpException] {  // Login is required
            agentClient.commandExecute(RegisterAsMaster) await 99.s
          } .status shouldEqual Unauthorized
          agentClient.login(Some(TestUserAndPassword)) await 99.s
          agentClient.commandExecute(RegisterAsMaster) await 99.s shouldEqual AgentCommand.Accepted  // Without Login, this registers all anonymous clients

          val order = Order(OrderId("TEST-ORDER"), SimpleTestWorkflow.id, Order.Ready, payload = Payload(Map("x" → "X")))
          agentClient.commandExecute(AttachOrder(order, TestAgentPath % "(initial)", SimpleTestWorkflow)) await 99.s shouldEqual AgentCommand.Accepted
          EventRequest.singleClass[OrderEvent](timeout = 10.seconds)
            .repeat(eventRequest ⇒ agentClient.mastersEvents(eventRequest).runAsync)
            {
              case Stamped(_, _, KeyedEvent(order.id, OrderDetachable)) ⇒
            }
          val processedOrder = agentClient.order(order.id) await 99.s
          assert(processedOrder == toExpectedOrder(order))
          agentClient.commandExecute(DetachOrder(order.id)) await 99.s shouldEqual AgentCommand.Accepted
          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))
          agentClient.commandExecute(AgentCommand.Terminate()) await 99.s
        }
      }
    }
  }

  for (testSpeed ← sys.props.get("test.speed")) s"Speed test $testSpeed orders × ${/*SimpleTestWorkflow.jobNodeCount*/"·"} jobs" in {
    val n = testSpeed.toInt
    provideAgentDirectory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir / "A.job.json").contentString = AJobConfiguration.asJson.toPrettyString
      (jobDir / "B.job.json").contentString = BJobConfiguration.asJson.toPrettyString
      val agentConf = AgentConfiguration.forTest(directory, ConfigFactory.parseString("""
         |jobscheduler.journal.sync = on
         |jobscheduler.journal.delay = 0ms
         |jobscheduler.journal.simulate-sync = 10ms
         |jobscheduler.journal.snapshot-log-period = 1.ms
         |jobscheduler.journal.snapshot-log-actor-limit = 10
         |""".stripMargin))
      val timeout = 1.hour
      RunningAgent.run(agentConf, timeout = Some(timeout)) { agent ⇒
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri.toString).closeWithCloser
          agentClient.login(Some(TestUserAndPassword)) await 99.s
          agentClient.commandExecute(RegisterAsMaster) await 99.s

          val orders = for (i ← 1 to n) yield
            Order(OrderId(s"TEST-ORDER-$i"), SimpleTestWorkflow.id, Order.Ready,
              Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "VERSION")), payload = Payload(Map("x" → "X")))

          val stopwatch = new Stopwatch
          agentClient.commandExecute(Batch(orders map { AttachOrder(_, SimpleTestWorkflow) })) await 99.s

          val awaitedOrderIds = (orders map { _.id }).toSet
          val ready = mutable.Set[OrderId]()
          while (
            agentClient.mastersEvents(EventRequest.singleClass[OrderEvent](timeout = timeout)) await 99.s match {
              case EventSeq.NonEmpty(stampeds) ⇒
                ready ++= stampeds map { _.value } collect { case KeyedEvent(orderId: OrderId, OrderDetachable) ⇒ orderId }
                ready != awaitedOrderIds
              case _ ⇒
                true
            }
          ) {}
          agentClient.commandExecute(Batch(orders map { o ⇒ DetachOrder(o.id) })) await 99.s
          info(stopwatch.itemsPerSecondString(n, "orders"))

          agentClient.commandExecute(AgentCommand.Terminate()) await 99.s
        }
      }
    }
  }
}

private object OrderAgentTest {
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private val AJobConfiguration =
    JobConfiguration(JobPath.NoId, JobScript(TestScript), Map("var1" → "AAA"), taskLimit = sys.runtime.availableProcessors)

  private val BJobConfiguration =
    JobConfiguration(JobPath.NoId, JobScript(TestScript), Map("var1" → "BBB"), taskLimit = sys.runtime.availableProcessors)

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      workflowPosition = order.workflowPosition.copy(position = Position(2)),
      attachedTo = Some(Order.AttachedTo.Detachable(TestAgentPath % "(initial)")),
      payload = Payload(Map("x" → "X", "result" → "TEST-RESULT-BBB")))
}
