package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.agent.data.views.{TaskHandlerOverview, TaskHandlerView, TaskOverview}
import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.tunnel.data.TunnelId
import java.net.InetAddress
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TaskWebServiceTest extends FreeSpec with WebServiceTest with TaskWebService {

  private val testAgentTaskId = AgentTaskId("1-123")

  protected val taskHandlerView = new TaskHandlerView {
    def overview = TaskHandlerOverview(
      currentTaskCount = 777,
      totalTaskCount = 999)

    def taskOverviews = List(taskOverview(testAgentTaskId))

    def taskOverview(id: AgentTaskId) = TaskOverview(
      testAgentTaskId,
      pid = None,
      TunnelId("99"),
      Instant.parse("2015-06-10T12:00:00Z"),
      startedByHttpIp = Some(InetAddress.getByName("127.1.2.3")),
      startMeta = StartTask.Meta(job = "/FOLDER/JOB", TaskId(123)),
      arguments = Some(TaskOverview.Arguments(
        language = "LANGUAGE",
        javaClassName = Some("JAVA.CLASS"),
        monitorCount = 3)))
  }

  "task" in {
    Get("/jobscheduler/agent/api/task") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskHandlerOverview] == taskHandlerView.overview)
      assert(responseAs[JsObject] == JsObject(
        "currentTaskCount" → JsNumber(777),
        "totalTaskCount" -> JsNumber(999)))
    }
  }

  "task/" in {
    Get("/jobscheduler/agent/api/task/") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[TaskOverview]] == taskHandlerView.taskOverviews)
      assert(responseAs[JsArray] == JsArray(
        JsObject(
          "id" → JsString("1-123"),
          "tunnelId" → JsString("99"),
          "startedAt" → JsString("2015-06-10T12:00:00Z"),
          "startedByHttpIp" → JsString("127.1.2.3"),
          "startMeta" → JsObject(
            "job" → JsString("/FOLDER/JOB"),
            "taskId" → JsString("123")
          ),
          "arguments" → JsObject(
            "language" → JsString("LANGUAGE"),
            "javaClassName" → JsString("JAVA.CLASS"),
            "monitorCount" → JsNumber(3)))))
    }
  }

  "task/1-123" in {
    Get("/jobscheduler/agent/api/task/1-123") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TaskOverview] == taskHandlerView.taskOverview(testAgentTaskId))
      assert(responseAs[JsObject] == JsObject(
          "id" → JsString("1-123"),
          "tunnelId" → JsString("99"),
          "startedAt" → JsString("2015-06-10T12:00:00Z"),
          "startedByHttpIp" → JsString("127.1.2.3"),
          "startMeta" → JsObject(
            "job" → JsString("/FOLDER/JOB"),
            "taskId" → JsString("123")
          ),
          "arguments" → JsObject(
            "language" → JsString("LANGUAGE"),
            "javaClassName" → JsString("JAVA.CLASS"),
            "monitorCount" → JsNumber(3))))
    }
  }
}
