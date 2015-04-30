package com.sos.scheduler.engine.agent.tests.api

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.tests.api.SchedulerAPIJavascriptIT._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder._
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.log.InfoLogEvent
import com.sos.scheduler.engine.test.SchedulerTestUtils._
import com.sos.scheduler.engine.test.scalatest.ScalaSchedulerTest
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
 * @author Andreas Liebert
 */
@RunWith(classOf[JUnitRunner])
final class SchedulerAPIJavascriptIT extends FreeSpec with ScalaSchedulerTest{

  private lazy val agentTcpPort = findRandomFreeTcpPort()
  private lazy val remoteSchedulerAddress = s"http://127.0.0.1:$agentTcpPort"
  private lazy val agent = new Agent(AgentConfiguration(httpPort = agentTcpPort, httpInterfaceRestriction = Some("127.0.0.1"))).closeWithCloser
  private val finishedOrderParametersPromise = Promise[Map[String, String]]()
  private val eventsPromise = Promise[immutable.Seq[Event]]()
  private lazy val taskLogLines = eventsPromise.successValue collect { case e: InfoLogEvent ⇒ e.message }

  protected override def onSchedulerActivated() = {
    val started = agent.start()
    scheduler executeXml <process_class name={s"$ProcessClassName"}
                                        remote_scheduler={s"$remoteSchedulerAddress"}/>
    awaitResult(started, 10.seconds)
  }

  "javascript job" in {
      val run = runJobFuture(JavascriptJobPath)
      val taskResult: TaskResult = awaitSuccess(run.result)
      taskResult.logString should include ("Hello world")
  }

  "shell job with javascript monitor" in {
    val run = runJobFuture(JavascriptMonitorJobPath)
    val taskResult: TaskResult = awaitSuccess(run.result)
    taskResult.logString should include ("Hello world")
    taskResult.logString should include ("##this is spooler_task_before##")
    taskResult.logString should include ("##this is spooler_process_before##")
    taskResult.logString should include ("##this is spooler_process_after##")
  }

}

object SchedulerAPIJavascriptIT {
  private val JavascriptJobPath = JobPath("/javascript")
  private val JavascriptMonitorJobPath = JobPath("/javascript_monitor")
  private val ProcessClassName = "test-agent"
}



