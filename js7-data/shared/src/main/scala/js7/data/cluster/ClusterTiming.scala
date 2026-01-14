package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.RichCirceObjectCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*

final case class ClusterTiming(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration):

  def checked: Checked[this.type] =
    if heartbeat.isPositive && heartbeatTimeout.isPositive then
      Right(this)
    else
      Left(Problem.pure("Invalid cluster timing values"))

  /** Timeout for `ClusterPassivelost`.
    *
    * Duration without heartbeat, after which the active node considers the passive node to be lost.
    * Shorter than `activeLostTimeout`.
    */
  def passiveLostTimeout: FiniteDuration =
    heartbeat + heartbeatTimeout

  /** Timeout for `ClusterFailedOver` (active node is lost).
    *
    * Duration without heartbeat, after which the passive node considers the active to be lost.
    * In case of a network lock-in, `ClusterFailedOver` event must be tried after
    * `ClusterPassiveLost`.
    * Because `ClusterPassiveLost` is checked every heartbeat,
    * we add a heartbeat (and an additional heartbeat for timing variation).
    */
  def activeLostTimeout: FiniteDuration =
    passiveLostTimeout + 2 * heartbeat

  def inhibitActivationDuration: FiniteDuration =
    activeLostTimeout + heartbeat

  /** Duration the ClusterWatch considers the last heartbeat of a node valid.
    */
  def clusterWatchHeartbeatValidDuration: FiniteDuration =
    passiveLostTimeout + heartbeat

  /** Duration, in which the ClusterWatchCounterpart must have received a response.
    * Otherwise, the request will be repeated.
    * Must be shorter than the difference between `passiveLostTimeout` and `activeLostTimeout`.
    */
  def clusterWatchReactionTimeout: FiniteDuration =
    heartbeat

  def clusterWatchHeartbeat: FiniteDuration =
    heartbeat

  def clusterWatchIdTimeout: FiniteDuration =
    heartbeat + 2 * heartbeatTimeout

  override def toString =
    s"ClusterTiming(${heartbeat.pretty}, ${heartbeatTimeout.pretty})"


object ClusterTiming:

  given Codec.AsObject[ClusterTiming] = deriveCodec[ClusterTiming].checked(_.checked)
