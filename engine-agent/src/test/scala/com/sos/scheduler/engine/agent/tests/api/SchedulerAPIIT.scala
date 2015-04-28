package com.sos.scheduler.engine.agent.tests.api

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.tests.api.SchedulerAPIIT._
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder._
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.log.InfoLogEvent
import com.sos.scheduler.engine.data.order.{OrderFinishedEvent, OrderStepEndedEvent, SuccessOrderStateTransition}
import com.sos.scheduler.engine.data.xmlcommands.OrderCommand
import com.sos.scheduler.engine.eventbus.EventSourceEvent
import com.sos.scheduler.engine.kernel.order.Order
import com.sos.scheduler.engine.test.EventBusTestFutures.implicits._
import com.sos.scheduler.engine.test.SchedulerTestUtils._
import com.sos.scheduler.engine.test.configuration.TestConfiguration
import com.sos.scheduler.engine.test.scalatest.ScalaSchedulerTest
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.io.Source

/**
 * @author Andreas Liebert
 */
@RunWith(classOf[JUnitRunner])
final class SchedulerAPIIT extends FreeSpec with ScalaSchedulerTest{

  protected override lazy val testConfiguration = TestConfiguration(
    testClass = getClass,
    mainArguments = List(s"-include-path=$IncludePath"))

  import controller.newEventPipe

  private lazy val agentTcpPort = findRandomFreeTcpPort()
  private lazy val remoteSchedulerAddress = s"http://127.0.0.1:$agentTcpPort"
  private lazy val agent = new Agent(AgentConfiguration(httpPort = agentTcpPort, httpInterfaceRestriction = Some("127.0.0.1"))).closeWithCloser
  private val finishedOrderParametersPromise = Promise[Map[String, String]]()
  private val eventsPromise = Promise[immutable.Seq[Event]]()
  private lazy val testTextFile = testEnvironment.configDirectory / TestTextFilename
  private lazy val taskLogLines = eventsPromise.successValue collect { case e: InfoLogEvent ⇒ e.message }

  protected override def onSchedulerActivated() = {
    val started = agent.start()
    scheduler executeXml VariablesJobElem
    scheduler executeXml <process_class name={s"$ProcessClassName"}
                                        remote_scheduler={s"$remoteSchedulerAddress"}
                                        max_processes={s"$MaxProcesses"}/>
    awaitResult(started, 10.seconds)
  }

  "spooler_log methods" in {

      val run = runJobFuture(JobPath("/log"))
      val taskResult: TaskResult = awaitSuccess(run.result)
      for (level <- LogJob.LogMessages.keySet) {
        val regularExpr = s"(?i)\\[$level\\]\\s+" + LogJob.LogMessages.get(level)
        taskResult.logString should include regex regularExpr
      }

      for (line <- Source.fromFile(testTextFile).getLines()) {
        taskResult.logString should include(line)
      }
      //taskResult.logString should include (LogJob.SpoolerCloseMessage)
      taskResult.logString should include (LogJob.SpoolerExitMessage)
      //taskResult.logString should include (LogJob.SpoolerInitMessage)
      taskResult.logString should include (LogJob.SpoolerOpenMessage)

  }

  "test job object" in {
    val run = runJobFuture(JobObjectsJobPath)
    val taskResult: TaskResult = awaitSuccess(run.result)

    for (mes <- JobObjectJob.UnwantedMessage.values()) {
      taskResult.logString should not include (mes.toString())
    }
    taskResult.logString should include (s"include_path=$IncludePath")
    taskResult.logString should include (s"process_class name=$ProcessClassName")
    taskResult.logString should include (s"process_class remote_scheduler=$remoteSchedulerAddress")
    taskResult.logString should include (s"process_class max_processes=$MaxProcesses")
  }

 "Run variables job via order" in {
    autoClosing(newEventPipe()) { eventPipe ⇒
      eventBus.onHotEventSourceEvent[OrderStepEndedEvent] {
        case EventSourceEvent(event, order: Order) ⇒ finishedOrderParametersPromise.success(order.parameters.toMap)
      }
      eventBus.awaitingKeyedEvent[OrderFinishedEvent](VariablesOrderKey) {
        scheduler executeXml OrderCommand(VariablesOrderKey, parameters = Map(OrderVariable.pair, OrderParamOverridesJobParam.pair))
      }
      eventsPromise.success(eventPipe.queued[Event])
    }
  }

  "Variables job exit code" in {
    assertResult(List(SuccessOrderStateTransition)) {
      eventsPromise.successValue collect { case OrderStepEndedEvent(VariablesOrderKey, stateTransition) ⇒ stateTransition }
    }
    eventBus.dispatchEvents()
  }

  "Order variable" in {
    assert(taskLogLines contains OrderVariable.expectedString)
  }

  "variables count" in {
    assert(taskLogLines contains TaskParamsCountPrefix+"2")
  }

  "variable substitution" in {
    val substitutedString = VariableSubstitutionString.replace("$"+JobParam.name,JobParam.value)
    assert(taskLogLines contains substitutedString)
  }

  "Order variable created in job" in {
    finishedOrderParametersPromise.successValue should contain (OrderVariableSetInJob.pair)
  }
}

object SchedulerAPIIT {
  private val VariablesJobchainPath = JobChainPath("/variables")
  private val VariablesJobPath = JobPath("/variables")
  private val VariablesOrderKey = VariablesJobchainPath orderKey "1"
  val OrderVariable = Variable("orderparam", "ORDERVALUE")
  val OrderVariableSetInJob = Variable("orderparaminjob", "qwertzui")
  val OrderParamOverridesJobParam = Variable("ORDEROVERRIDESJOBPARAM", "ORDEROVERRIDESJOBVALUE")
  val TaskParamsCountPrefix="Taskparamscount:"
  private val JobParam = Variable("testparam", "PARAM-VALUE")
  val VariableSubstitutionString = "aaaa $"+JobParam.name+" aaaa"
  val TestTextFilename = "logText.txt"
  val IncludePath = "fooo"
  val JobObjectsJobPath = JobPath("/job_object")
  val ProcessClassName = "test-agent"
  private val MaxProcesses = 23


  final case class Variable(name: String, value: String) {
    override def toString = name
    def expectedString = s"$name=$value"
    def pair = name → value
  }

  private val VariablesJobElem =
    <job name={VariablesJobPath.name} process_class="test-agent" stop_on_error="false" order="yes">
      <params>
        <param name={JobParam.name} value={JobParam.value}/>
        <param name={OrderParamOverridesJobParam.name} value="OVERRIDDEN-JOB-VALUE"/>
      </params>
      <script language="java" java_class="com.sos.scheduler.engine.agent.tests.api.VariablesJob"/>
    </job>
}
