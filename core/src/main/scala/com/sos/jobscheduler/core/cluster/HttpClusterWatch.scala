package com.sos.jobscheduler.core.cluster

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.session.{HttpAutoRelogin, HttpSessionApi}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.web.HttpClient.HttpException
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.cluster.ClusterWatch.isClusterWatchProblem
import com.sos.jobscheduler.core.cluster.HttpClusterWatch._
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import io.circe._
import monix.eval.Task

final class HttpClusterWatch(
  protected val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi with HttpAutoRelogin
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
                loginUntilReachable(userAndPassword, delays, onlyIfNotLoggedIn = true) >>
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
