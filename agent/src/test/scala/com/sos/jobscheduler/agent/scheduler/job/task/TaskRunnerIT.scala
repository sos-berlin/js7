package com.sos.jobscheduler.agent.scheduler.job.task

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.scheduler.job.JobConfiguration
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunnerIT._
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.xmlElemToSource
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.data.jobnet.{JobPath, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.Order.Good
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.system.StdoutStderr._
import com.sos.jobscheduler.taskserver.task.process.StdoutStderrWriter
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class TaskRunnerIT extends FreeSpec with BeforeAndAfterAll {

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
        JobPath("/TEST"),
        xmlElemToSource(
          <job>
            <params>
              <param name="var1" value="VALUE1"/>
            </params>
            <script language="shell">{TestScript}</script>
          </job>))
      measureTime(10, "TaskRunner") {
        val order = Order(
          OrderId("TEST"),
          NodeKey(JobnetPath("/JOBCHAIN"), NodeId("NODE")),
          Order.InProcess,
          Map("a" → "A"))
        implicit val x = injector.instance[StandardAgentTaskFactory]
        val taskRunner = newTaskRunner(jobConfiguration)
        val stdoutStderrWriter = new TestStdoutStderrWriter
        val ended = taskRunner.processOrder(order, stdoutStderrWriter) andThen { case _ ⇒ taskRunner.terminate() } await 30.s
        assert(ended == TaskStepSucceeded(
          variablesDiff = MapDiff.addedOrUpdated(Map("result" → "TEST-RESULT-VALUE1")),
          Good(returnValue = true))
        )
        if (newTaskRunner.isInstanceOf[LegacyApiTaskRunner.Factory]) {  // TODO LegacyApiTaskRunner does not use StdoutStderrWriter
          assert(stdoutStderrWriter.string(Stdout).isEmpty)
          assert(stdoutStderrWriter.string(Stderr).isEmpty)
        } else {
          val nl = System.lineSeparator
          assert(stdoutStderrWriter.string(Stdout) == s"Hej!${nl}var1=VALUE1$nl")
          assert(stdoutStderrWriter.string(Stderr) == s"THIS IS STDERR$nl")
        }
      }
    }
  }
}

object TaskRunnerIT {
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

  private final class TestStdoutStderrWriter extends StdoutStderrWriter {
    private val builders = (for (t ← StdoutStderrType.values) yield t → new StringBuilder).toMap

    def chunkSize = 10

    def writeChunk(t: StdoutStderrType, chunk: String) =
      builders(t) ++= chunk

    def string(t: StdoutStderrType) = builders(t).toString
  }
}
