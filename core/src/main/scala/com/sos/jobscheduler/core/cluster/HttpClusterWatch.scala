package com.sos.jobscheduler.core.cluster

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{Uri => AkkaUri}
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.session.{HttpAutoRelogin, HttpSessionApi}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.web.HttpClient.HttpException
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.cluster.ClusterWatch.isClusterWatchProblem
import com.sos.jobscheduler.core.cluster.HttpClusterWatch._
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import io.circe._
import monix.eval.Task
import scala.collection.immutable.Seq

final class HttpClusterWatch(
  protected val baseUri: AkkaUri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val actorSystem: ActorSystem)
extends ClusterWatchApi with AkkaHttpClient with HttpSessionApi with HttpAutoRelogin
{
  protected def httpClient = this

  protected def uriPrefixPath = "/agent"

  protected val sessionUri = baseUri + "/agent/api/session"

  protected def name = "ClusterWatch"

  private val clusterUri = baseUri + "/agent/api/master/cluster"

  def applyEvents(from: Uri, events: Seq[ClusterEvent], reportedClusterState: ClusterState): Task[Checked[Completed]] =
    liftProblem(
      retryUntilReachable(
        post[ClusterWatchMessage, JsonObject](clusterUri, ClusterWatchEvents(from, events, reportedClusterState))
      ).onErrorRestartLoop(()) { (throwable, _, retry) =>
          // TODO onErrorRestartLoop duplicate with retryUntilReachable ?
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

  def heartbeat(from: Uri, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    liftProblem(
      loginUntilReachable(userAndPassword, loginDelays(), onlyIfNotLoggedIn = true) >>
        post[ClusterWatchMessage, JsonObject](clusterUri, ClusterWatchHeartbeat(from, reportedClusterState))
          .map((_: JsonObject) => Completed))

  def get: Task[Checked[ClusterState]] =
    liftProblem(
      get[ClusterState](clusterUri))
}

object HttpClusterWatch
{
  private val logger = Logger(getClass)
}
