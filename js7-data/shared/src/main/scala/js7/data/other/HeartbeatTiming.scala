package js7.data.other

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.RichCirceObjectCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import scala.concurrent.duration.FiniteDuration

final case class HeartbeatTiming(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration):
  def checked: Checked[HeartbeatTiming] =
    if heartbeat.isZeroOrBelow || heartbeatTimeout.isZeroOrBelow then
      Left(Problem.pure("Invalid heartbeat timing values"))
    else
      Right(this)

  def longHeartbeatTimeout: FiniteDuration =
    heartbeat + heartbeatTimeout

  def heartbeatValidDuration: FiniteDuration =
    heartbeat + heartbeatTimeout / 2

  override def toString = s"HeartbeatTiming(${heartbeat.pretty}, ${heartbeatTimeout.pretty})"


object HeartbeatTiming:
  def checked(heartbeat: FiniteDuration, heartbeatTimeout: FiniteDuration)
  : Checked[HeartbeatTiming] =
    new HeartbeatTiming(heartbeat, heartbeatTimeout).checked

  implicit val jsonCodec: Codec.AsObject[HeartbeatTiming] =
    deriveCodec[HeartbeatTiming].checked(_.checked)

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
