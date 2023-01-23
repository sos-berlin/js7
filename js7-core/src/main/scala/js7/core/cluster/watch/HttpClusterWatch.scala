package js7.core.cluster.watch

import akka.actor.ActorSystem
import io.circe.*
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.io.https.HttpsConfig
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient.HttpException
import js7.base.web.Uri
import js7.cluster.watch.api.AnyClusterWatch
import js7.cluster.watch.api.ClusterWatchProblems.isClusterWatchProblem
import js7.common.http.AkkaHttpClient
import js7.core.cluster.watch.HttpClusterWatch.*
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchMessage}
import js7.data.node.NodeId
import js7.data.session.HttpSessionApi
import monix.eval.Task
import scala.annotation.unused

final class HttpClusterWatch(
  ownNodeId: NodeId,
  val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val httpsConfig: HttpsConfig,
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi
with SessionApi.HasUserAndPassword
with AnyClusterWatch
{
  protected val sessionUri = Uri(s"$baseUri/agent/api/session")
  private val clusterUri = Uri(s"$baseUri/agent/api/clusterWatch")

  override def close(): Unit = {
    logOpenSession()
    super.close()
  }

  protected def httpClient = this

  protected def uriPrefixPath = "/agent"

  protected def name = "ClusterWatch"

  def checkClusterState(clusterState: HasNodes, clusterWatchIdChangeAllowed: Boolean)
  : Task[Checked[None.type]] =
    Task.defer {
      val msg = ClusterWatchCheckState(RequestId(0), CorrelId.current, ownNodeId, clusterState)
      liftProblem(
        loginUntilReachable(Iterator.continually(ErrorDelay), onlyIfNotLoggedIn = true)
          .*>(postMsg(msg))
          .onErrorRestartLoop(())(onError)
          .as(None))
    }

  def applyEvent(event: ClusterEvent, clusterState: HasNodes): Task[Checked[None.type]] =
    Task.defer {
      val msg = ClusterWatchCheckEvent(RequestId(0), CorrelId.current, ownNodeId, event, clusterState)
      liftProblem(
        retryUntilReachable()(
          post[ClusterWatchMessage, JsonObject](clusterUri, msg)
        ).onErrorRestartLoop(())(onError)
          .map((_: JsonObject) => None))
    }

  private def postMsg(msg: ClusterWatchMessage): Task[Completed] =
    post[ClusterWatchMessage, JsonObject](clusterUri, msg)
      .map((_: JsonObject) => Completed)

  private def onError[A](throwable: Throwable, @unused unit: Unit, retry: Unit => Task[A]) =
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

  def clusterState: Task[Checked[ClusterState]] =
    liftProblem(
      get[ClusterState](clusterUri))
}

object HttpClusterWatch
{
  private val ErrorDelay = 1.s  // TODO
  private val logger = Logger(getClass)
}
