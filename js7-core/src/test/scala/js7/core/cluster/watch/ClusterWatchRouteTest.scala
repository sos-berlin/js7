package js7.core.cluster.watch

import io.circe.JsonObject
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.generic.SecretString
import js7.base.log.CorrelId
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient.`x-js7-request-id`
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.web.session.SessionInit
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchRequest}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchRouteTest extends OurTestSuite with ScalatestRouteTest with ClusterWatchRoute
{
  coupleScribeWithSlf4j()

  protected implicit val scheduler = Scheduler.traced
  protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)

  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(10.s)
  private val controllerId = ControllerId("CONTROLLER")

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  private val route = pathSegment("cluster") {
    clusterWatchRouteFor(controllerId, new ClusterWatchSession {
      type User = SimpleUser
      val sessionInit =
        SessionInit(SessionToken(SecretString("?")), SimpleUser(controllerId.toUserId))
    })
  }

  "Get for unknown ControllerId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == BadRequest &&
        entityAs[Problem] == Problem("No ClusterState registered for 'ControllerId:CONTROLLER'"))
    }
  }

  private val setting = ClusterSetting(
    Map(
      NodeId("A") -> Uri("https://A"),
      NodeId("B") -> Uri("https://B")),
    NodeId("A"),
    ClusterTiming(10.s, 20.s),
    clusterWatchId = None,
    Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))))

  "Post" in {
    Post[ClusterWatchRequest]("/cluster",
      ClusterWatchCheckEvent(
        RequestId(1),
        CorrelId("CorrelId"),
        NodeId("A"),
        ClusterNodesAppointed(setting),
        ClusterState.NodesAppointed(setting))
    ) ~>
      Accept(`application/json`) ~>
      addHeader(`x-js7-request-id`.name, "#1") ~>
      route ~>
      check {
        assert(status == OK && entityAs[JsonObject] == JsonObject.empty)
      }
  }

  "Get for known ControllerId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK &&
        entityAs[ClusterState] == ClusterState.NodesAppointed(setting))
    }
  }
}
