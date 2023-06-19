package js7.data.command

import io.circe.Codec
import js7.base.circeutils.CirceUtils.{DecodeWithDefaults, deriveConfiguredCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport

final case class SuspensionMode(kill: Option[CancellationMode.Kill] = None)

object SuspensionMode
{
  val standard = new SuspensionMode()
  val kill = new SuspensionMode(Some(CancellationMode.Kill()))
  val killImmediately = new SuspensionMode(Some(CancellationMode.Kill(immediately = true)))

  def apply(kill: Option[CancellationMode.Kill] = None): SuspensionMode =
    kill match {
      case None =>
        standard

      case Some(CancellationMode.Kill(false, None)) =>
        SuspensionMode.kill

      case Some(CancellationMode.Kill(true, None)) =>
        killImmediately

      case Some(_) =>
        new SuspensionMode(kill)
    }

  intelliJuseImport(DecodeWithDefaults)
  implicit val jsonCodec: Codec.AsObject[SuspensionMode] =
    deriveConfiguredCodec[SuspensionMode]
}
