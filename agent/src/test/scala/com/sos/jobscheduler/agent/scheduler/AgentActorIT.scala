package com.sos.jobscheduler.agent.scheduler

import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, DetachOrder, GetOrders, RegisterAsMaster}
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
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.NodeKey
import com.sos.jobscheduler.data.workflow.test.TestSetting._
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
      TestAgentActorProvider.provide { provider ⇒
        import provider.{agentDirectory, eventCollector, executeCommand}
        for (jobPath ← TestJobPaths)
          (agentDirectory / "config" / "live" / s"${jobPath.name}.job.xml").xml =
            <job tasks="100">
              <params>
                <param name="from_job" value="FROM-JOB"/>
              </params>
              <script language="shell">{AScript}</script>
            </job>
        withCloser { implicit closer ⇒
          val lastEventId = eventCollector.lastEventId
          (provider.agentActor ? AgentActor.Input.Start).mapTo[AgentActor.Output.Ready.type] await 99.s
          executeCommand(RegisterAsMaster) await 99.s
          val stopwatch = new Stopwatch
          val orderIds = for (i ← 0 until n) yield OrderId(s"TEST-ORDER-$i")
          orderIds map (orderId ⇒ executeCommand(AttachOrder(TestOrder.copy(id = orderId), TestAgentPath, TestWorkflow))) await 99.s
          for (orderId ← orderIds)
            eventCollector.whenKeyedEvent[OrderEvent.OrderDetachable.type](EventRequest.singleClass(after = lastEventId, 90.s), orderId) await 99.s
          info(stopwatch.itemsPerSecondString(n, "Orders"))

          val GetOrders.Response(orders) = executeCommand(GetOrders) await 99.s
          assert(orders.toSet ==
            orderIds.map(orderId ⇒ Order(
              orderId,
              NodeKey(TestWorkflow.path, END.id),
              Order.Ready,
              Some(Order.AttachedTo.Detachable(TestAgentPath)),
              payload = TestOrder.payload.copy(
                variables = TestOrder.payload.variables + ("result" → "TEST-RESULT-FROM-JOB"))
            )).toSet)

          (for (orderId ← orderIds) yield executeCommand(DetachOrder(orderId))) await 99.s
          for (orderId ← orderIds)
            eventCollector.whenKeyedEvent[OrderEvent.OrderDetached.type](EventRequest.singleClass(after = lastEventId, 90.s), orderId) await 99.s
        }
        executeCommand(AgentCommand.Terminate()) await 99.s
      }
    }
  }
}

object AgentActorIT {
  private val AScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo var1=%SCHEDULER_PARAM_VAR1%
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_FROM_JOB% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo "var1=$SCHEDULER_PARAM_VAR1"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_FROM_JOB" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin
}
