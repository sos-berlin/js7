package js7.controller.web.controller.api

import js7.base.test.{OurTestSuite, TestCatsEffect}
import js7.cluster.web.ClusterRoute
import js7.common.pekkohttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.web.session.SimpleSession
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.{ClusterCommand, ClusterNodeState, ClusterState, ClusterWatchingCommand}
import js7.data.event.Stamped
import js7.data.node.NodeId
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.CatsEffectExtensions.right
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class ClusterRouteTest extends OurTestSuite, RouteTester, ClusterRoute:

  protected type OurSession = SimpleSession

  protected def actorSystem = system
  protected def whenShuttingDown = Deferred.unsafe

  protected val nodeId = NodeId("NODE-ID")
  protected def clusterNodeIsBackup = false
  protected val checkedClusterState = IO.right(Stamped(1, ClusterState.Empty))
  protected def clusterWatchRequestStream = throw new NotImplementedError
  protected def nextCusterWatchMessage = throw new NotImplementedError

  protected def eventWatch =
    throw new NotImplementedError

  protected def executeClusterCommand(command: ClusterCommand) =
    throw new NotImplementedError

  protected def executeClusterWatchingCommand(cmd: ClusterWatchingCommand) =
    throw new NotImplementedError

  private lazy val route: Route =
    pathSegment("cluster"):
      clusterRoute

  "/cluster" in:
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check:
      assert(status == OK
        && responseAs[Stamped[ClusterState]] == Stamped(1, ClusterState.Empty))

  "/cluster?return=ClusterNodeState" in:
    Get("/cluster?return=ClusterNodeState") ~> Accept(`application/json`) ~> route ~> check:
      assert(status == OK
        && responseAs[ClusterNodeState] == ClusterNodeState(nodeId, false, ClusterState.Empty))
