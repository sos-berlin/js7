package com.sos.jobscheduler.agent.web.views

import akka.actor.Props
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.{BaseAgentTask, TaskRegister, TaskRegisterActor}
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.agent.web.views.TaskWebServiceTest._
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.Futures.NoFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import java.time.Instant
import org.scalatest.FreeSpec
import scala.collection.immutable
import scala.concurrent.duration.DurationInt
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class TaskWebServiceTest extends FreeSpec with WebServiceTest with TaskWebService {

  protected lazy val taskRegister = {
    val actor = actorSystem.actorOf(Props { new TaskRegisterActor(AgentConfiguration.forTest(), new TimerService(idleTimeout = Some(1.s))) })
    new TaskRegister(actor)(99.seconds)
  }

  protected lazy val executionContext = actorSystem.dispatcher

  override def beforeAll() = {
    taskRegister.add(new BaseAgentTask {
      def overview = testTaskOverview(TestAgentTaskId)
      def pidOption = overview.pid
      def jobPath = overview.jobPath
      def id = overview.taskId
      def terminated = NoFuture
      def sendProcessSignal(signal: ProcessSignal) = {}
    })
    super.beforeAll()
  }

  "task" in {
    Get("/jobscheduler/agent/api/task") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskRegisterOverview] == TestOverview)
      assert(responseAs[JsObject] == JsObject(
        "currentTaskCount" → JsNumber(1),
        "totalTaskCount" -> JsNumber(1)))
    }
  }

  "task/" in {
    Get("/jobscheduler/agent/api/task/") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[TaskOverview]] == TestTaskOverviews)
      assert(responseAs[JsArray] == JsArray(
        JsObject(
          "taskId" → JsString("1-123"),
          "pid" → JsNumber(123),
          "startedAt" → JsString("2015-06-10T12:00:00Z"),
          "jobPath" → JsString("/FOLDER/JOB"))))
    }
  }

  "task/1-123" in {
    Get("/jobscheduler/agent/api/task/1-123") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskOverview] == testTaskOverview(TestAgentTaskId))
      assert(responseAs[JsObject] == JsObject(
          "taskId" → JsString("1-123"),
          "pid" → JsNumber(123),
          "startedAt" → JsString("2015-06-10T12:00:00Z"),
          "jobPath" → JsString("/FOLDER/JOB")))
    }
  }
}

private object TaskWebServiceTest {
  private val TestAgentTaskId = AgentTaskId("1-123")

  private val TestOverview = TaskRegisterOverview(
    currentTaskCount = 1,
    totalTaskCount = 1)

  private val TestTaskOverviews = List(testTaskOverview(TestAgentTaskId))

  private def testTaskOverview(id: AgentTaskId) = TaskOverview(
    jobPath = JobPath("/FOLDER/JOB"),
    TestAgentTaskId,
    pid = Some(Pid(123)),
    Instant.parse("2015-06-10T12:00:00Z"))
}
