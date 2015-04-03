package com.sos.scheduler.engine.agenttest

import com.sos.scheduler.engine.agent.AgentConfiguration
import com.sos.scheduler.engine.agent.main.Main
import com.sos.scheduler.engine.agenttest.AgentIT._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.job.{JobPath, ReturnCode, TaskEndedEvent}
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.log.InfoLogEvent
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.data.order.{OrderFinishedEvent, OrderStepEndedEvent}
import com.sos.scheduler.engine.data.xmlcommands.OrderCommand
import com.sos.scheduler.engine.eventbus.EventSourceEvent
import com.sos.scheduler.engine.kernel.order.Order
import com.sos.scheduler.engine.test.EventBusTestFutures.implicits._
import com.sos.scheduler.engine.test.SchedulerTestUtils._
import com.sos.scheduler.engine.test.scalatest.ScalaSchedulerTest
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentIT extends FreeSpec with ScalaSchedulerTest {

  import controller.{newEventPipe, toleratingErrorCodes, toleratingErrorLogEvent}

  private lazy val agentTcpPort = findRandomFreeTcpPort()
  private lazy val agentApp = new Main(AgentConfiguration(httpPort = agentTcpPort, httpInterfaceRestriction = Some("127.0.0.1"))).closeWithCloser
  private val eventsPromise = Promise[immutable.Seq[Event]]()
  private lazy val shellOutput: immutable.Seq[String] = taskLogLines collect { case ScriptOutputRegex(o) ⇒ o.trim }
  private lazy val taskLogLines = eventsPromise.successValue collect { case e: InfoLogEvent ⇒ e.message }
  private val finishedOrderParametersPromise = Promise[Map[String, String]]()

  protected override def onSchedulerActivated() = {
    val started = agentApp.start()
    scheduler executeXml TestJobElem
    scheduler executeXml <process_class name="test-agent" remote_scheduler={s"http://127.0.0.1:$agentTcpPort"}/>
    awaitResult(started, 10.seconds)
  }

  "Run shell job via order" in {
    autoClosing(newEventPipe()) { eventPipe ⇒
      toleratingErrorCodes(Set(MessageCode("SCHEDULER-280"))) { // "Process terminated with exit code ..."
        val orderKey = TestJobchainPath orderKey "1"
        eventBus.onHotEventSourceEvent[OrderStepEndedEvent] {
          case EventSourceEvent(event, order: Order) ⇒ finishedOrderParametersPromise.success(order.parameters.toMap)
        }
        eventBus.awaitingKeyedEvent[OrderFinishedEvent](orderKey) {
          scheduler executeXml OrderCommand(orderKey, parameters = Map(OrderVariable.pair, OrderParamOverridesJobParam.pair))
        }
      }
      eventsPromise.success(eventPipe.queued[Event])
    }
  }

  "Shell script exit code" in {
    assertResult(List(TestReturnCode)) {
      eventsPromise.successValue collect { case TaskEndedEvent(_, TestJobPath, returnCode) ⇒ returnCode }
    }
    eventBus.dispatchEvents()
  }

  "Order variable" in {
    assert(shellOutput contains OrderVariable.expectedString)
  }

  "Order variable overrides job variable with same name" in {
    assert(shellOutput contains OrderParamOverridesJobParam.expectedString)
  }

  "Job parameter" in {
    assert(shellOutput contains JobParam.expectedString)
  }

  "Job environment variable" in {
    assert(shellOutput contains EnvironmentVariable.expectedString)
  }

  "Other environment variables are unchanged" in {
    val path = (sys.env collectFirst { case ("PATH" | "Path", v) ⇒ v }).head
    assert(shellOutput contains s"PATH=$path")
  }

  "stdout in task log" in {
    assert(shellOutput contains "STDOUT AGENT ECHO")
  }

  "stderr in task log" in {
    assert(shellOutput contains "STDERR AGENT ECHO")
  }

  "Job.stateText" in {
    assert(job(TestJobPath).stateText == s"!$FirstStdoutLine")
  }

  "SCHEDULER_RETURN_VALUES" in {
    finishedOrderParametersPromise.successValue should contain (ChangedVariable.pair)
  }

  "Multiple concurrent tasks" in {
    pending
  }

  "Shell with monitor - unexpected process termination of one monitor does not disturb the other task" in {
    val file = createTempFile("sos", ".tmp") withCloser Files.delete
    toleratingErrorCodes(Set(MessageCode("SCHEDULER-202"), MessageCode("SCHEDULER-280"), MessageCode("WINSOCK-10054"), MessageCode("ERRNO-32"), MessageCode("Z-REMOTE-101"))) {
      val test = runJobFuture(JobPath("/no-crash"), variables = Map(SignalName → file.toString))
      awaitSuccess(runJobFuture(JobPath("/crash"), variables = Map(SignalName → file.toString)).result).logString should include ("SCHEDULER-202")
      awaitSuccess(test.result).logString should include ("SPOOLER_PROCESS_AFTER")
    }
  }

  "Exception in Monitor" in {
    toleratingErrorLogEvent({ e ⇒ e.codeOption == Some(MessageCode("SCHEDULER-280")) || (e.message startsWith "COM-80020009 java.lang.RuntimeException: MONITOR EXCEPTION") }) {
    //toleratingErrorCodes(Set(MessageCode("COM-80020009"), MessageCode("SCHEDULER-280"))) {
      runJobAndWaitForEnd(JobPath("/throwing-monitor"))
    }
  }
}

object AgentIT {
  private val TestJobchainPath = JobChainPath("/test")
  private val TestJobPath = JobPath("/test")
  private val TestReturnCode = ReturnCode(42)
  private val FirstStdoutLine = "FIRST STDOUT LINE"
  private val OrderVariable = Variable("ORDERPARAM", "ORDERVALUE")
  private val OrderParamOverridesJobParam = Variable("ORDEROVERRIDESJOBPARAM", "ORDEROVERRIDESJOBVALUE")
  private val JobParam = Variable("TESTPARAM", "PARAM-VALUE")
  private val EnvironmentVariable = Variable("TESTENV", "ENV-VALUE")
  private val SchedulerVariables = List(OrderVariable, OrderParamOverridesJobParam, JobParam)
  private val ScriptOutputRegex = "[^!]*!(.*)".r  // Our test script output start with '!'
  private val ChangedVariable = Variable("CHANGED", "CHANGED-VALUE")
  val SignalName = "signalFile"

  private case class Variable(name: String, value: String) {
    override def toString = name
    def expectedString = s"$name=$value"
    def pair = name → value
  }

  private val TestJobElem =
    <job name={TestJobPath.name} process_class="test-agent" stop_on_error="false">
      <params>
        <param name={JobParam.name} value={JobParam.value}/>
        <param name={OrderParamOverridesJobParam.name} value="OVERRIDDEN-JOB-VALUE"/>
      </params>
      <environment>
        <variable name={EnvironmentVariable.name} value={EnvironmentVariable.value}/>
      </environment>
      <script language="shell">{
        def variableToEcho(v: Variable) = {
          import v._
          val envName = paramToEnvName(name)
          "echo !" + (if (isWindows) s"$name=%$envName%" else s"$name=$$$envName") + "\n"
        }
        (
          if (isWindows) s"""
            |@echo off
            |echo !$FirstStdoutLine
            |echo !$EnvironmentVariable=%$EnvironmentVariable%
            |echo !PATH=%Path%
            |if "%SCHEDULER_RETURN_VALUES%" == "" goto :noReturnValues
            |    echo ${ChangedVariable.name}=${ChangedVariable.value} >> %SCHEDULER_RETURN_VALUES%
            |:noReturnValues
            |""".stripMargin
          else s"""
            |echo !$FirstStdoutLine
            |echo !$EnvironmentVariable=$$$EnvironmentVariable
            |echo !PATH=$$PATH
            |[ -n "$$SCHEDULER_RETURN_VALUES" ] && echo ${ChangedVariable.name}=${ChangedVariable.value} >> $$SCHEDULER_RETURN_VALUES
            |""".stripMargin
        ) +
        (SchedulerVariables map variableToEcho).mkString + s"""
          |echo !STDOUT AGENT ECHO
          |echo !STDERR AGENT ECHO 1>&2
          |exit ${TestReturnCode.toInt}
          |""".stripMargin
      }</script>
    </job>

  private def paramToEnvName(name: String) = s"SCHEDULER_PARAM_$name"
}
