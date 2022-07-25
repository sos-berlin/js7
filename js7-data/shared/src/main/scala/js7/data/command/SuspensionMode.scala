package js7.data.command

import io.circe.generic.extras.Configuration
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec

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

  private implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val jsonCodec = deriveConfiguredCodec[SuspensionMode]
}
