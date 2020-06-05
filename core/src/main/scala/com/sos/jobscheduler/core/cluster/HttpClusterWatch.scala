package js7.core.cluster

import akka.actor.ActorSystem
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.session.HttpSessionApi
import js7.base.utils.ScalaUtils.RichThrowable
import js7.base.web.HttpClient.HttpException
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient
import js7.common.scalautil.Logger
import js7.core.cluster.ClusterWatch.isClusterWatchProblem
import js7.core.cluster.HttpClusterWatch._
import js7.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import io.circe._
import monix.eval.Task

final class HttpClusterWatch(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi
{
  protected def httpClient = this

  protected def uriPrefixPath = "/agent"

  protected val sessionUri = Uri(s"$baseUri/agent/api/session")

  protected def trustStoreRef = None
  protected def keyStoreRef = None

  protected def name = "ClusterWatch"

  private val clusterUri = Uri(s"$baseUri/agent/api/master/cluster")

  def applyEvents(from: ClusterNodeId, events: Seq[ClusterEvent], reportedClusterState: ClusterState, force: Boolean = false)
  : Task[Checked[Completed]] =
    liftProblem(
      retryUntilReachable(
        post[ClusterWatchMessage, JsonObject](clusterUri, ClusterWatchEvents(from, events, reportedClusterState, force))
      ) .onErrorRestartLoop(()) { (throwable, _, retry) =>
          logger.warn(throwable.toStringWithCauses)
          throwable match {
            case throwable: HttpException if throwable.problem exists isClusterWatchProblem =>
              Task.raiseError(throwable)
            case _ =>
              val delays = loginDelays()
              Task.sleep(delays.next()) >>
                loginUntilReachable(delays, onlyIfNotLoggedIn = true) >>
                retry(())
          }
        }
        .map((_: JsonObject) => Completed))

  def heartbeat(from: ClusterNodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    liftProblem(
      retryUntilReachable(
        post[ClusterWatchMessage, JsonObject](clusterUri, ClusterWatchHeartbeat(from, reportedClusterState))
      ).map((_: JsonObject) => Completed))

  def get: Task[Checked[ClusterState]] =
    liftProblem(
      get[ClusterState](clusterUri))
}

object HttpClusterWatch
{
  private val logger = Logger(getClass)
}
