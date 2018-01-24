package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, Batch, DetachOrder, Login, RegisterAsMaster}
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider.provideAgentDirectory
import com.sos.jobscheduler.agent.tests.OrderAgentTest._
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetachable
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentTest extends FreeSpec {

  "AgentCommand AttachOrder" in {
    provideAgentDirectory { directory ⇒
      val jobDir = directory / "config" / "live"
      (jobDir / AJobPath.toXmlFile).xml = AJobXml
      (jobDir / BJobPath.toXmlFile).xml = BJobXml
      val agentConf = AgentConfiguration.forTest(Some(directory))
      RunningAgent.run(agentConf, timeout = Some(99.s)) { agent ⇒
        withCloser { implicit closer ⇒
          implicit val actorSystem = newActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri.toString).closeWithCloser

          agentClient.executeCommand(RegisterAsMaster) await 99.s shouldEqual AgentCommand.Accepted  // Without Login, this registers all anonymous clients

          val order = Order(OrderId("TEST-ORDER"), TestWorkflow.path, Order.Ready, payload = Payload(Map("x" → "X")))
          agentClient.executeCommand(AttachOrder(order, TestAgentPath, TestWorkflow.workflow)) await 99.s shouldEqual AgentCommand.Accepted
          EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 10.s).repeat(agentClient.mastersEvents) {
            case Stamped(_, KeyedEvent(order.id, OrderDetachable)) ⇒
          }
          val processedOrder = agentClient.order(order.id) await 99.s
          assert(processedOrder == toExpectedOrder(order))
          agentClient.executeCommand(DetachOrder(order.id)) await 99.s shouldEqual AgentCommand.Accepted
          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))
          agentClient.executeCommand(AgentCommand.Terminate()) await 99.s
        }
      }
    }
  }

  for (testSpeed ← sys.props.get("test.speed")) s"Speed test $testSpeed orders × ${/*TestWorkflow.workflow.jobNodeCount*/"·"} jobs" in {
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
            Order(OrderId(s"TEST-ORDER-$i"), TestWorkflow.path, Order.Ready, payload = Payload(Map("x" → "X")))

          val stopwatch = new Stopwatch
          agentClient.executeCommand(Batch(orders map { AttachOrder(_, TestWorkflow.workflow) })) await 99.s

          val awaitedOrderIds = (orders map { _.id }).toSet
          val ready = mutable.Set[OrderId]()
          while (
            agentClient.mastersEvents(EventRequest.singleClass[OrderEvent](after = EventId.BeforeFirst, timeout = timeout)) await 99.s match {
              case EventSeq.NonEmpty(stampeds) ⇒
                ready ++= stampeds map { _.value } collect { case KeyedEvent(orderId: OrderId, OrderDetachable) ⇒ orderId }
                ready != awaitedOrderIds
              case _ ⇒
                true
            }
          ) {}
          agentClient.executeCommand(Batch(orders map { o ⇒ DetachOrder(o.id) })) await 99.s
          info(stopwatch.itemsPerSecondString(n, "orders"))

          agentClient.executeCommand(AgentCommand.Terminate()) await 99.s
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

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      workflowPosition = order.workflowPosition.copy(position = Position(2)),
      attachedTo = Some(Order.AttachedTo.Detachable(TestAgentPath)),
      payload = Payload(Map("x" → "X", "result" → "TEST-RESULT-BBB")))
}
