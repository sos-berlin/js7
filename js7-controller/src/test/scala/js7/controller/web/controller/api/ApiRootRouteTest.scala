package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.BuildInfo
import js7.base.auth.UserId
import js7.base.time.Timestamp
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.system.startup.StartUp
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerId, ControllerMetaState, ControllerOverview, ControllerState}
import js7.data.event.{EventId, JournalState, JournaledState}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends AnyFreeSpec with RouteTester with ApiRootRoute
{
  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected def controllerState = Task.pure(Right(ControllerState.empty.copy(
    eventId = EventId(1001),
    standards = JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Empty),
    controllerMetaState = ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      timezone = "Europe/Berlin"))))
  protected def totalRunningSince = now - 1.hour

  private def route: Route =
    pathSegment("api") {
      apiRootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[ControllerOverview]
      assert(overview.id == controllerId)
      assert(overview.version == BuildInfo.prettyVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.initiallyStartedAt == Some(Timestamp("2019-05-24T12:00:00Z")))
      assert(overview.startedAt == StartUp.startedAt)
      assert(overview.totalRunningTime >= 1.hour && overview.totalRunningTime <= 1.hour + 1.minute)
      assert(overview.orderCount == Some(0))
    }
  }
}
