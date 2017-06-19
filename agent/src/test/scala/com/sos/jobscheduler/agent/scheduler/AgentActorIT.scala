package com.sos.jobscheduler.agent.scheduler

import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.{AttachJobnet, AttachOrder, DetachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.scheduler.AgentActorIT._
import com.sos.jobscheduler.agent.scheduler.order.TestAgentActorProvider
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentActorIT extends FreeSpec {

  private implicit val askTimeout = Timeout(60.seconds)

  for (n ← List(10) ++ (sys.props contains "test.speed" option 1000)) {
    s"AgentActorIT, $n orders" in {
      TestAgentActorProvider.provide { env ⇒
        import env.{agentDirectory, eventCollector, executeCommand}
        (agentDirectory / "config" / "live" / "test.job.xml").xml =
          <job tasks="100">
            <params>
              <param name="var1" value="VALUE1"/>
            </params>
            <script language="shell">{AScript}</script>
          </job>
        withCloser { implicit closer ⇒
          val lastEventId = eventCollector.lastEventId
          (env.agentActor ? AgentActor.Input.Start).mapTo[AgentActor.Output.Started.type] await 99.s
          executeCommand(RegisterAsMaster) await 99.s
          executeCommand(AttachJobnet(AJobnet)) await 99.s
          val stopwatch = new Stopwatch
          val orderIds = for (i ← 0 until n) yield OrderId(s"TEST-ORDER-$i")
          (for (orderId ← orderIds) yield
            executeCommand(AttachOrder(Order(
              orderId,
              NodeKey(AJobnet.path, NodeId("100")),
              Order.Waiting,
              Map("a" → "A"))))
          ) await 99.s
          for (orderId ← orderIds)
            eventCollector.whenKeyedEvent[OrderEvent.OrderReady.type](EventRequest.singleClass(after = lastEventId, 90.s), orderId) await 99.s
          info(stopwatch.itemsPerSecondString(n, "Orders"))
          (for (orderId ← orderIds) yield executeCommand(DetachOrder(orderId))) await 99.s
        }
      }
    }
  }
}

object AgentActorIT {
  private val TestAgentId = AgentPath("/TEST-AGENT")
  val AJobPath = JobPath("/test")
  val AJobnet = Jobnet(
    JobnetPath("/A"),
    NodeId("100"),
    List(
      Jobnet.JobNode(NodeId("100"), TestAgentId, AJobPath, onSuccess = NodeId("END"), onFailure = NodeId("FAILED")),
      Jobnet.EndNode(NodeId("FAILED")),
      Jobnet.EndNode(NodeId("END"))))
  private val AScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo var1=%SCHEDULER_PARAM_VAR1%
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo "var1=$SCHEDULER_PARAM_VAR1"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin
}
