package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.{BaseAgentTask, TaskRegister, TaskRegisterActor}
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.agent.web.views.TaskWebServiceTest._
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.job.JobPath
import io.circe.Json
import java.time.Instant
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

/**
 * @author Joacim Zschimmer
 */
final class TaskWebServiceTest extends FreeSpec with WebServiceTest with TaskWebService {

  protected lazy val taskRegister = {
    val actor = actorSystem.actorOf(TaskRegisterActor.props(AgentConfiguration.forTest(), new TimerService(idleTimeout = Some(1.s))))
    new TaskRegister(actor)(99.seconds)
  }

  protected def executionContext = scheduler
  protected def scheduler = Scheduler.global

  private val route =
    pathSegments("agent/api/task") {
      taskRoute
    }

  override def beforeAll() = {
    taskRegister.add(new BaseAgentTask {
      def overview = testTaskOverview(TestAgentTaskId)
      def pidOption = overview.pid
      def jobPath = overview.jobPath
      def id = overview.taskId
      def terminated = Promise().future
      def sendProcessSignal(signal: ProcessSignal) = {}
    }) await 99.s
    super.beforeAll()
  }

  "task" in {
    Get("/agent/api/task") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskRegisterOverview] == TestOverview)
      assert(responseAs[Json] == Json.obj(
        "currentTaskCount" → Json.fromInt(1),
        "totalTaskCount" → Json.fromInt(1)))
    }
  }

  "task/" in {
    Get("/agent/api/task/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[TaskOverview]] == TestTaskOverviews)
      assert(responseAs[Json] == Json.fromValues(List(
        Json.obj(
          "taskId" → Json.fromString("1-123"),
          "pid" → Json.fromInt(123),
          "startedAt" → Json.fromLong(1433937600000L),  //Json.fromString("2015-06-10T12:00:00Z"),
          "jobPath" → Json.fromString("/FOLDER/JOB")))))
    }
  }

  "task/1-123" in {
    Get("/agent/api/task/1-123") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskOverview] == testTaskOverview(TestAgentTaskId))
      assert(responseAs[Json] == Json.obj(
          "taskId" → Json.fromString("1-123"),
          "pid" → Json.fromInt(123),
          "startedAt" → Json.fromLong(1433937600000L),  //Json.fromString("2015-06-10T12:00:00Z"),
          "jobPath" → Json.fromString("/FOLDER/JOB")))
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
    Instant.parse("2015-06-10T12:00:00Z").toTimestamp)
}
