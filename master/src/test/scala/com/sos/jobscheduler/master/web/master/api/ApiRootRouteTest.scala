package js7.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.BuildInfo
import js7.base.auth.UserId
import js7.base.time.Timestamp
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.http.CirceJsonSupport._
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.filebased.Repo
import js7.data.master.{MasterFileBaseds, MasterId}
import js7.master.data.{MasterOverview, MasterState}
import js7.master.data.MasterSnapshots.MasterMetaState
import js7.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends AnyFreeSpec with RouteTester with ApiRootRoute
{
  protected val masterId = MasterId("TEST-MASTER")
  protected def isShuttingDown = false
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected def masterState = Task.pure(Right(MasterState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Empty),
    MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Repo.ofJsonDecoder(MasterFileBaseds.jsonCodec),
    Map.empty,
    Map.empty)))
  protected def totalRunningSince = now - 1.hour

  private def route: Route =
    pathSegment("api") {
      apiRootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[MasterOverview]
      assert(overview.id == masterId)
      assert(overview.version == BuildInfo.prettyVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.startedAt == Some(Timestamp("2019-05-24T12:00:00Z")))
      assert(overview.totalRunningTime >= 1.hour && overview.totalRunningTime <= 1.hour + 1.minute)
      assert(overview.orderCount == Some(0))
    }
  }
}
