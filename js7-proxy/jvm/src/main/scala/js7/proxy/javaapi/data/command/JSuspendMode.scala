package js7.proxy.javaapi.data.command

import java.util.Optional
import js7.data.command.SuspendMode
import js7.data.command.CancelMode.Kill
import js7.proxy.javaapi.data.common.JavaWrapper
import js7.proxy.javaapi.data.workflow.position.JWorkflowPosition
import scala.jdk.OptionConverters._

final case class JSuspendMode(asScala: SuspendMode)
extends JavaWrapper
{
  protected type AsScala = SuspendMode
}

object JSuspendMode
{
  /** Kill a running job (with SIGTERM if possible). */
  def kill: JSuspendMode =
    kill(immediately = false)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(immediately: Boolean): JSuspendMode =
    kill(immediately, Optional.empty)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(
    immediately: Boolean,
    @Nonnull position: Optional[JWorkflowPosition]
  ): JSuspendMode =
    JSuspendMode(SuspendMode(Some(
      Kill(immediately = immediately, position.toScala.map(_.asScala)))))
}
