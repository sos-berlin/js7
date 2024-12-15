package js7.controller.web.controller.api

import cats.effect.{Deferred, IO}
import js7.base.BuildInfo
import js7.base.auth.UserId
import js7.base.system.startup.StartUp
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.{Timestamp, Timezone}
import js7.common.pekkohttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerId, ControllerMetaState, ControllerOverview, ControllerState}
import js7.data.event.{EventId, JournalState, SnapshotableState}
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends OurTestSuite, RouteTester, ApiRootRoute:

  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected val whenShuttingDown = Deferred.unsafe

  protected def controllerState = IO.pure(Right(ControllerState.empty.copy(
    eventId = EventId(1001),
    standards = SnapshotableState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Empty),
    controllerMetaState = ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      ts"2019-05-24T12:00:00Z",
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
      assert(overview.initiallyStartedAt == Some(ts"2019-05-24T12:00:00Z"))
      assert(overview.startedAt == StartUp.startedAt)
      assert(overview.totalRunningTime >= 1.hour && overview.totalRunningTime <= 1.hour + 1.minute)
      assert(overview.orderCount == Some(0))
