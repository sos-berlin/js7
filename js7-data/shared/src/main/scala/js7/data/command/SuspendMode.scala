package js7.data.command

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec

final case class SuspendMode(kill: Option[CancelMode.Kill] = None)

object SuspendMode
{
  val default = new SuspendMode()
  val kill = new SuspendMode(Some(CancelMode.Kill()))
  val killImmediately = new SuspendMode(Some(CancelMode.Kill(immediately = true)))

  def apply(kill: Option[CancelMode.Kill] = None): SuspendMode =
    kill match {
      case None =>
        default

      case Some(CancelMode.Kill(false, None)) =>
        SuspendMode.kill

      case Some(CancelMode.Kill(true, None)) =>
        killImmediately

      case Some(_) =>
        new SuspendMode(kill)
    }

  private implicit val customConfig = withDefaults
  implicit val jsonCodec = deriveConfiguredCodec[SuspendMode]
}
