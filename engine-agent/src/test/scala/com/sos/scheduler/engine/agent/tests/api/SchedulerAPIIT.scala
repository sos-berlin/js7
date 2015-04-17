package com.sos.scheduler.engine.agent.tests.api

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder._
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.test.ImplicitTimeout
import com.sos.scheduler.engine.test.SchedulerTestUtils._
import com.sos.scheduler.engine.test.scalatest.ScalaSchedulerTest
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._
import scala.collection.JavaConversions._
import scala.concurrent.duration._

/**
 * @author Andreas Liebert
 */
@RunWith(classOf[JUnitRunner])
final class SchedulerAPIIT extends FreeSpec with ScalaSchedulerTest{

  private lazy val agentTcpPort = findRandomFreeTcpPort()
  private lazy val agent = new Agent(AgentConfiguration(httpPort = agentTcpPort, httpInterfaceRestriction = Some("127.0.0.1"))).closeWithCloser

  protected override def onSchedulerActivated() = {
    val started = agent.start()
    //scheduler executeXml <process_class name="test-agent" remote_scheduler={s"http://127.0.0.1:$agentTcpPort"}/>
    scheduler executeXml <process_class name="test-agent" remote_scheduler={s"http://gollum.sos:5555"}/>
    awaitResult(started, 10.seconds)
  }

  "test logger" in {


    for(level <- ApiJob.LogMessages.keySet()){
      val run = runJobFuture(JobPath("/api"), variables = Map("log_level" → level))

      val taskResult: TaskResult = level match {
        case "error" => awaitCompletion(run.result).get
        case _ => awaitSuccess(run.result)
      }


      val regularExpr = s"(?i)\\[$level\\]\\s+"+ApiJob.LogMessages.get(level);
      taskResult.logString should include regex regularExpr
    }
  }
}
