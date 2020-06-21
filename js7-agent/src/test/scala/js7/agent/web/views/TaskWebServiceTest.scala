package js7.agent.web.views

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import io.circe.Json
import js7.agent.data.AgentTaskId
import js7.agent.data.views.{TaskOverview, TaskRegisterOverview}
import js7.agent.task.{BaseAgentTask, TaskRegister, TaskRegisterActor}
import js7.agent.web.test.WebServiceTest
import js7.agent.web.views.TaskWebServiceTest._
import js7.base.circeutils.CirceUtils._
import js7.base.process.ProcessSignal
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.CirceJsonSupport._
import js7.common.process.Processes.Pid
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.data.job.JobKey
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.executable.WorkflowJob
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class TaskWebServiceTest extends AnyFreeSpec with WebServiceTest with TaskWebService
{
  protected def whenShuttingDown = Future.never
  protected lazy val taskRegister = {
    val actor = actorSystem.actorOf(TaskRegisterActor.props(None))
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
      def jobKey = overview.jobKey
      def id = overview.taskId
      def terminated = Promise().future
      def sendProcessSignal(signal: ProcessSignal) = {}
    }) await 99.s
    super.beforeAll()
  }

  "task" in {
    Get("/agent/api/task") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskRegisterOverview] == TestOverview)
      assert(responseAs[Json] == json"""
        {
          "currentTaskCount": 1,
          "totalTaskCount": 1
        }""")
    }
  }

  "task/" in {
    Get("/agent/api/task/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[Seq[TaskOverview]] == TestTaskOverviews)
      assert(responseAs[Json] == json"""
        [
          {
            "taskId": "1-123",
            "pid": 123,
            "startedAt": 1433937600000,
            "jobKey": {
              "workflowId": {
                "path": "/WORKFLOW",
                "versionId": "VERSION"
              },
              "jobName": "JOB"
            }
          }
        ]""")
    }
  }

  "task/1-123" in {
    Get("/agent/api/task/1-123") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskOverview] == testTaskOverview(TestAgentTaskId))
      assert(responseAs[Json] == json"""
        {
          "taskId": "1-123",
          "pid": 123,
          "startedAt": 1433937600000,
          "jobKey": {
            "workflowId": {
              "path": "/WORKFLOW",
              "versionId": "VERSION"
            },
            "jobName": "JOB"
          }
        }""")
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
    JobKey(WorkflowPath("/WORKFLOW") ~ "VERSION", WorkflowJob.Name("JOB")),
    TestAgentTaskId,
    pid = Some(Pid(123)),
    Timestamp.parse("2015-06-10T12:00:00Z"))
}
