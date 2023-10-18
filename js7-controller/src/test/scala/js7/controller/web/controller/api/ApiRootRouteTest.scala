package js7.controller.web.controller.api

import js7.base.BuildInfo
import js7.base.auth.UserId
import js7.base.test.OurTestSuite
import js7.base.time.{Timestamp, Timezone}
import js7.common.pekkohttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.system.startup.StartUp
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerId, ControllerMetaState, ControllerOverview, ControllerState}
import js7.data.event.{EventId, JournalState, SnapshotableState}
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends OurTestSuite, RouteTester, ApiRootRoute:
  
  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.traced
  protected def controllerState = Task.pure(Right(ControllerState.empty.copy(
    eventId = EventId(1001),
    standards = SnapshotableState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Empty),
    controllerMetaState = ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      Timezone("Europe/Berlin")))))
  protected def totalRunningSince = now - 1.hour

  private def route: Route =
    pathSegment("api"):
      apiRootRoute

  "/api" in:
    Get("/api") ~> Accept(`application/json`) ~> route ~> check:
      val overview = responseAs[ControllerOverview]
      assert(overview.id == controllerId)
      assert(overview.version == BuildInfo.prettyVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.initiallyStartedAt == Some(Timestamp("2019-05-24T12:00:00Z")))
      assert(overview.startedAt == StartUp.startedAt)
      assert(overview.totalRunningTime >= 1.hour && overview.totalRunningTime <= 1.hour + 1.minute)
      assert(overview.orderCount == Some(0))
