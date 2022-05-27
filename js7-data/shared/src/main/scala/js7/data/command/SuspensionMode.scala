package js7.data.command

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

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

  private implicit val customConfig = withDefaults
  implicit val jsonCodec = deriveConfiguredCodec[SuspensionMode]
}
