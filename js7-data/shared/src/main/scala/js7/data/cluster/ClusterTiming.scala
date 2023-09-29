package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.cluster.ClusterTiming.*
import scala.concurrent.duration.*

final case class ClusterTiming(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration)
{
  checkedUnit(heartbeat, heartbeatTimeout).orThrow

  /** Duration the ClusterWatch considers the last heartbeat valid. */
  def clusterWatchHeartbeatValidDuration =
    passiveLostTimeout + heartbeat

  /** Duration without heartbeat, after which the passive node may be lost.
   * Shorter than `activeLostTimeout`.
   */
  def passiveLostTimeout =
    heartbeat + heartbeatTimeout

  /** Duration without heartbeat, after which the active node may be lost.
   * failOverTimeout is longer than passiveLostTimeout.
   * In case of a network lock-in, FailedOver must occur after PassiveLost
   * Because PassiveLost is checked every ClusterWatch heartbeat,
   * we add a heartbeat (and an additional heartbeat for timing variation).
   */
  def activeLostTimeout =
    passiveLostTimeout + 2 * heartbeat

  /** Duration, the ClusterWatch client must have received a response.
   * Otherwise, the request will be repeated.
   * Must be shorter then the difference between `passiveLostTimeout` and `activeLostTimeout`.
   */
  def clusterWatchReactionTimeout  =
    heartbeat

  def inhibitActivationDuration =
    activeLostTimeout + heartbeat

  def clusterWatchHeartbeat =
    heartbeat

  def clusterWatchIdTimeout =
    heartbeat + 2 * heartbeatTimeout

  override def toString = s"ClusterTiming(${heartbeat.pretty}, ${heartbeatTimeout.pretty})"
}

object ClusterTiming
{
  def checked(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration): Checked[ClusterTiming] =
    for (_ <- checkedUnit(heartbeat, heartbeatTimeout)) yield
      new ClusterTiming(heartbeat, heartbeatTimeout)

  private def checkedUnit(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration) =
    (heartbeat.isPositive && heartbeatTimeout.isPositive) !!
      Problem.pure("Invalid cluster timing values")

  implicit val jsonCodec: Codec.AsObject[ClusterTiming] = deriveCodec

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
}
