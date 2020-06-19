package js7.core.cluster

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import js7.base.auth.SessionToken
import js7.base.generic.{Completed, SecretString}
import js7.base.session.SessionCommand
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.https.HttpsConfig
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkautils.ProvideActorSystem
import js7.common.http.CirceJsonSupport._
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.core.cluster.HttpClusterWatchTest._
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterNodeId, ClusterState}
import js7.data.controller.ControllerId
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

final class HttpClusterWatchTest extends AnyFreeSpec with BeforeAndAfterAll with ProvideActorSystem
{
  override protected def config = ConfigFactory.empty
  private val controllerId = ControllerId("CONTROLLER")

  private val controllersClusterRoute = new ControllersClusterRoute {
    protected def scheduler = Scheduler.global
    protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)
    def route = controllerClusterRoute(controllerId)
  }.route

  private lazy val server = new AkkaWebServer.ForTest(
    actorSystem,
    route = decodeRequest {
      testSessionRoute ~
      pathSegments("agent/api/controller/cluster") {
        controllersClusterRoute
      }
    }
  ).closeWithCloser

  override def beforeAll() = {
    super.beforeAll()
    server.start() await 99.s
  }

  "HttpClusterWatch" in {
    val clusterWatch = new HttpClusterWatch(server.localUri, userAndPassword = None, HttpsConfig.empty, actorSystem)
    val idToUri = Map(ClusterNodeId("A") -> Uri("http://A"), ClusterNodeId("B") -> Uri("http://B"))
    val primaryUri = ClusterNodeId("A")
    val expectedClusterState = ClusterState.NodesAppointed(idToUri, primaryUri)
    assert(clusterWatch.applyEvents(primaryUri, ClusterNodesAppointed(idToUri, primaryUri) :: Nil, expectedClusterState).await(99.s) ==
      Right(Completed))
    assert(clusterWatch.get.await(99.s) == Right(expectedClusterState))
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
              SessionCommand.Login.LoggedIn(SessionToken(SecretString("SESSION")))
            case _: SessionCommand.Logout =>
              SessionCommand.Response.Accepted
          }
          complete(response)
        }
      }
    }
  }
}
