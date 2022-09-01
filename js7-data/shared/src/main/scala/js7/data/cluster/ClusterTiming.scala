package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.cluster.ClusterTiming._
import scala.concurrent.duration.FiniteDuration

final case class ClusterTiming(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration)
{
  checkedUnit(heartbeat, longHeartbeatTimeout).orThrow

  def longHeartbeatTimeout = heartbeat + heartbeatTimeout

  override def toString = s"ClusterTiming(${heartbeat.pretty}, ${heartbeatTimeout.pretty})"
}

object ClusterTiming
{
  def checked(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration): Checked[ClusterTiming] =
    for (_ <- checkedUnit(heartbeat, heartbeatTimeout)) yield
      new ClusterTiming(heartbeat, heartbeatTimeout)

  private def checkedUnit(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration) =
    if (heartbeat.isZeroOrBelow || heartbeatTimeout.isZeroOrBelow)
      Left(Problem.pure("Invalid cluster timing values"))
    else
      Right(())

  implicit val jsonCodec = deriveCodec[ClusterTiming]

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
}
