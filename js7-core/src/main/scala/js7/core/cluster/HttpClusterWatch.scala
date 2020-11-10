package js7.core.cluster

import akka.actor.ActorSystem
import io.circe._
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.HttpClient.HttpException
import js7.base.web.Uri
import js7.common.akkahttp.https.HttpsConfig
import js7.common.http.AkkaHttpClient
import js7.common.scalautil.Logger
import js7.core.cluster.ClusterWatch.isClusterWatchProblem
import js7.core.cluster.HttpClusterWatch._
import js7.data.cluster.ClusterState
import js7.data.node.NodeId
import js7.data.session.HttpSessionApi
import monix.eval.Task

final class HttpClusterWatch(
  val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  httpsConfig: HttpsConfig,
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi
{
  protected def httpClient = this

  protected def uriPrefixPath = "/agent"

  protected val sessionUri = Uri(s"$baseUri/agent/api/session")

  protected def name = "ClusterWatch"

  protected def keyStoreRef = httpsConfig.keyStoreRef

  protected def trustStoreRefs = httpsConfig.trustStoreRefs

  private val clusterUri = Uri(s"$baseUri/agent/api/clusterWatch")

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] =
    liftProblem(
      retryUntilReachable()(
        post[ClusterWatchMessage, JsonObject](clusterUri, clusterWatchEvents)
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

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    liftProblem(
      (loginUntilReachable(Iterator.continually(ErrorDelay), onlyIfNotLoggedIn = true) >>
        post[ClusterWatchMessage, JsonObject](clusterUri, ClusterWatchHeartbeat(from, reportedClusterState))
          .map((_: JsonObject) => Completed)
      ) .onErrorRestartLoop(()) { (throwable, _, retry) =>
          throwable match {
            case throwable: HttpException if throwable.problem exists isClusterWatchProblem =>
              Task.raiseError(throwable)
            case _ =>
              logger.warn(throwable.toStringWithCauses)
              retry(()).delayExecution(ErrorDelay)
          }
        })

  def get: Task[Checked[ClusterState]] =
    liftProblem(
      get[ClusterState](clusterUri))
}

object HttpClusterWatch
{
  private val ErrorDelay = 1.s  // TODO
  private val logger = Logger(getClass)
}
