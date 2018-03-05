package com.sos.jobscheduler.agent.scheduler.job.task

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunnerTest._
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, ShellReturnValuesProvider}
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.process.Processes.newTemporaryShellFile
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.xmlElemToSource
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.taskserver.task.process.{RichProcess, StdChannels}
import java.io.Writer
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class TaskRunnerTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val injector = Guice.createInjector(new AgentModule(AgentConfiguration.forTest()))

  override protected def afterAll() = {
    injector.instance[Closer].close()
    super.afterAll()
  }

  for ((newTaskRunner, name) ← Array(
      injector.instance[SimpleShellTaskRunner.Factory] → "SimpleShellTaskRunner",
      injector.instance[LegacyApiTaskRunner.Factory] → "LegacyApiTaskRunner"))
  {
    name in {
      val jobConfiguration = JobConfiguration.parseXml(
        JobPath("/TEST") % "VERSION",
        xmlElemToSource(
          <job>
            <params>
              <param name="var1" value="VALUE1"/>
            </params>
            <script language="shell">{TestScript}</script>
          </job>)).force
      val shellFile = newTemporaryShellFile("TaskRunnerTest")
      shellFile.contentString = jobConfiguration.script.string.trim
      val shellReturnValuesProvider = new ShellReturnValuesProvider
      val taskConfiguration = TaskConfiguration(jobConfiguration, shellFile, shellReturnValuesProvider)
      info(measureTime(10, "TaskRunner") {
        val order = Order(
          OrderId("TEST"),
          WorkflowPath("/JOBCHAIN") % "VERSION",
          Order.InProcess,
          payload = Payload(Map("a" → "A")))
        implicit val x = injector.instance[StandardAgentTaskFactory]
        val taskRunner = newTaskRunner(taskConfiguration) await 99.s
        val stdoutWriter = new TestStdoutStderrWriter
        val stderrWriter = new TestStdoutStderrWriter
        val stdChannels = new StdChannels(charBufferSize = 10, stdoutWriter = stdoutWriter, stderrWriter = stderrWriter)
        val ended = taskRunner.processOrder(order, stdChannels) andThen { case _ ⇒ taskRunner.terminate() } await 30.s
        assert(ended == TaskStepSucceeded(MapDiff(Map("result" → "TEST-RESULT-VALUE1")), ReturnCode(0)))
        if (newTaskRunner.isInstanceOf[LegacyApiTaskRunner.Factory]) {  // TODO LegacyApiTaskRunner does not use StdoutStderrWriter
          assert(stdoutWriter.string.isEmpty)
          assert(stderrWriter.string.isEmpty)
        } else {
          val nl = System.lineSeparator
          assert(stdoutWriter.string == s"Hej!${nl}var1=VALUE1$nl")
          assert(stderrWriter.string == s"THIS IS STDERR$nl")
        }
      }.toString)
      RichProcess.tryDeleteFiles(shellFile :: shellReturnValuesProvider.file :: Nil)
    }
  }
}

object TaskRunnerTest {
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo THIS IS STDERR>&2
      |echo var1=%SCHEDULER_PARAM_VAR1%
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo THIS IS STDERR >&2
      |echo "var1=$SCHEDULER_PARAM_VAR1"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private final class TestStdoutStderrWriter extends Writer {
    private val stringBuilder = new StringBuilder

    def chunkSize = 10


    def write(chars: Array[Char], offset: Int, length: Int) =
      stringBuilder.appendAll(chars, offset, length)

    def string = stringBuilder.toString

    def flush() = {}

    def close() = {}
  }
}
