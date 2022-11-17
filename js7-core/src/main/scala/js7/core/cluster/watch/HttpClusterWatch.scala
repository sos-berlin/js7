package js7.core.cluster.watch

import akka.actor.ActorSystem
import io.circe.*
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient.HttpException
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient
import js7.core.cluster.watch.ClusterWatch.isClusterWatchProblem
import js7.core.cluster.watch.HttpClusterWatch.*
import js7.data.cluster.ClusterState
import js7.data.node.NodeId
import js7.data.session.HttpSessionApi
import monix.eval.Task

final class HttpClusterWatch(
  val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val httpsConfig: HttpsConfig,
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi
with SessionApi.HasUserAndPassword
{
  override def close(): Unit = {
    logOpenSession()
    super.close()
  }

  protected def httpClient = this

  protected def uriPrefixPath = "/agent"

  protected val sessionUri = Uri(s"$baseUri/agent/api/session")

  protected def name = "ClusterWatch"

  private val clusterUri = Uri(s"$baseUri/agent/api/clusterWatch")

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] =
    liftProblem(
      retryUntilReachable()(
        post[ClusterWatchMessage, JsonObject](clusterUri, clusterWatchEvents)
      ) .onErrorRestartLoop(()) { (throwable, _, retry) =>
          throwable match {
            case throwable: HttpException if throwable.problem exists isClusterWatchProblem =>
              Task.raiseError(throwable)
            case HttpException.HasProblem(InvalidSessionTokenProblem) =>
              loginUntilReachable(Iterator.continually(ErrorDelay))
                .delayExecution(ErrorDelay) >>
                retry(())
            case _ =>
              logger.warn(throwable.toStringWithCauses)
              retry(()).delayExecution(ErrorDelay)
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
            case HttpException.HasProblem(InvalidSessionTokenProblem) =>
              loginUntilReachable(Iterator.continually(ErrorDelay))
                .delayExecution(ErrorDelay) >>
                retry(())
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
