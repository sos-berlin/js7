package js7.data.command

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults

final case class SuspensionMode(
  resetState: Boolean = false,
  kill: Option[CancellationMode.Kill] = None)


object SuspensionMode:
  val standard = new SuspensionMode(resetState = false, kill = None)
  val kill: SuspensionMode =
    new SuspensionMode(resetState = false, kill = Some(CancellationMode.Kill()))
  val killImmediately: SuspensionMode =
    new SuspensionMode(resetState = false, kill = Some(CancellationMode.Kill(immediately = true)))

  def apply(
    resetState: Boolean = false,
    kill: Option[CancellationMode.Kill] = None)
  : SuspensionMode =
    (resetState, kill) match
      case (false, None) =>
        standard

      case (false, Some(CancellationMode.Kill(false, None))) =>
        SuspensionMode.kill

      case (false, Some(CancellationMode.Kill(true, None))) =>
        killImmediately

      case _ =>
        new SuspensionMode(resetState = resetState, kill = kill)

  implicit val jsonCodec: Codec.AsObject[SuspensionMode] =
    deriveCodecWithDefaults[SuspensionMode]
