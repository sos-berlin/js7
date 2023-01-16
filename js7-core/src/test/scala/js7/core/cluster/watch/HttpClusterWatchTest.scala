package js7.core.cluster.watch

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.generic.{Completed, SecretString}
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionCommand
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.SessionInit
import js7.common.akkautils.ProvideActorSystem
import js7.core.cluster.watch.HttpClusterWatchTest.*
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll

final class HttpClusterWatchTest extends OurTestSuite with BeforeAndAfterAll with ProvideActorSystem
{
  override protected def config = ConfigFactory.empty
  private val controllerId = ControllerId("CONTROLLER")

  private lazy val clusterWatchRoute = {
    trait HasRoute {
      def route: Route
    }
    new ClusterWatchRoute with HasRoute {
      protected def scheduler = Scheduler.traced
      protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)

      def route = clusterWatchRouteFor(
        controllerId,
        new ClusterWatchSession
        {
          type User = SimpleUser

          def sessionInit =
            SessionInit(SessionToken(SecretString("?")), SimpleUser(controllerId.toUserId))
        }
      )
    }.route
  }

  private lazy val server = AkkaWebServer.forTest()(
    decodeRequest {
      testSessionRoute ~
      pathSegments("agent/api/clusterWatch") {
        clusterWatchRoute
      }
    }
  )(actorSystem).closeWithCloser

  override def beforeAll() = {
    super.beforeAll()
    server.start await 99.s
  }

  override def afterAll() = {
    closer.close()
    super.afterAll()
  }

  "HttpClusterWatch" in {
    val clusterWatch = new HttpClusterWatch(
      NodeId("A"), server.localUri, userAndPassword = None, HttpsConfig.empty, actorSystem)
    val primaryId = NodeId("A")
    val setting = ClusterSetting(
      Map(
        NodeId("A") -> Uri("http://A"),
        NodeId("B") -> Uri("http://B")),
      activeId = primaryId,
      ClusterTiming(10.s, 20.s),
      clusterWatchId = None,
      Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))))
    val expectedClusterState = ClusterState.NodesAppointed(setting)
    assert(clusterWatch
      .applyEvent(ClusterNodesAppointed(setting), expectedClusterState)
      .await(99.s) == Right(Completed))
    assert(clusterWatch.clusterState.await(99.s) == Right(expectedClusterState))
  }
}

object HttpClusterWatchTest
{
  private def testSessionRoute: Route =
    decodeRequest/*decompress*/ {
    (pathSegments("agent/api/session") & pathEnd) {
      // test dummy
      testSessionRoute
      post {
        entity(as[SessionCommand]) { cmd =>
          val response: SessionCommand.Response = cmd match {
            case _: SessionCommand.Login =>
              SessionCommand.Login.LoggedIn(
                SessionToken(SecretString("SESSION")),
                js7Version = Js7Version)

            case _: SessionCommand.Logout =>
              SessionCommand.Response.Accepted
          }
          complete(response)
        }
      }
    }
  }
}
