package com.sos.jobscheduler.core.cluster

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.session.SessionCommand
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTask
import com.sos.jobscheduler.core.cluster.HttpClusterWatchTest._
import com.sos.jobscheduler.data.cluster.ClusterEvent.ClusterNodesAppointed
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.master.MasterId
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

final class HttpClusterWatchTest extends FreeSpec with BeforeAndAfterAll with ProvideActorSystem
{
  override protected def config = ConfigFactory.empty
  private val masterId = MasterId("MASTER")

  private val mastersClusterRoute = new MastersClusterRoute {
    protected def scheduler = Scheduler.global
    protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)
    def route = masterClusterRoute(masterId)
  }.route

  private lazy val server = new AkkaWebServer.ForTest(
    actorSystem,
    route = decodeRequest {
      testSessionRoute ~
      pathSegments("agent/api/master/cluster") {
        mastersClusterRoute
      }
    }
  ).closeWithCloser

  override def beforeAll() = {
    super.beforeAll()
    server.start() await 99.s
  }

  "HttpClusterWatch" in {
    val clusterWatch = new HttpClusterWatch(server.localUri, userAndPassword = None, actorSystem)
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
