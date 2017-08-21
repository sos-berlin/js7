package com.sos.jobscheduler.agent.web.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.data.jobnet.JobPath
import java.time.Instant
import org.scalatest.FreeSpec
import scala.collection.immutable
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class TaskWebServiceTest extends FreeSpec with WebServiceTest with TaskWebService {

  private val testAgentTaskId = AgentTaskId("1-123")

  //TODO Test fehlt

  //protected val taskRegisterOverview = new TaskHandlerView {
  //  def overview = TaskRegisterOverview(
  //    currentTaskCount = 777,
  //    totalTaskCount = 999)
  //
  //  def taskOverviews = List(taskOverview(testAgentTaskId))
  //
  //  def taskOverview(id: AgentTaskId) = TaskOverview(
  //    jobPath = JobPath("/FOLDER/JOB"),
  //    testAgentTaskId,
  //    pid = None,
  //    Instant.parse("2015-06-10T12:00:00Z"))
  //}
  //
  //"task" in {
  //  Get("/jobscheduler/agent/api/task") ~> Accept(`application/json`) ~> route ~> check {
  //    assert(responseAs[TaskRegisterOverview] == taskRegisterOverview.overview)
  //    assert(responseAs[JsObject] == JsObject(
  //      "currentTaskCount" → JsNumber(777),
  //      "totalTaskCount" -> JsNumber(999)))
  //  }
  //}
  //
  //"task/" in {
  //  Get("/jobscheduler/agent/api/task/") ~> Accept(`application/json`) ~> route ~> check {
  //    assert(responseAs[immutable.Seq[TaskOverview]] == taskRegisterOverview.taskOverviews)
  //    assert(responseAs[JsArray] == JsArray(
  //      JsObject(
  //        "id" → JsString("1-123"),
  //        "pid" -> JsNumber(123),
  //        "startedAt" → JsString("2015-06-10T12:00:00Z"),
  //        "jobPath" → JsString("/FOLDER/JOB"))))
  //  }
  //}
  //
  //"task/1-123" in {
  //  Get("/jobscheduler/agent/api/task/1-123") ~> Accept(`application/json`) ~> route ~> check {
  //    assert(responseAs[TaskOverview] == taskRegisterOverview.taskOverview(testAgentTaskId))
  //    assert(responseAs[JsObject] == JsObject(
  //        "id" → JsString("1-123"),
  //        "pid" → JsNumber(123),
  //        "startedAt" → JsString("2015-06-10T12:00:00Z"),
  //        "jobPath" → JsString("/FOLDER/JOB")))
  //  }
  //}
}
