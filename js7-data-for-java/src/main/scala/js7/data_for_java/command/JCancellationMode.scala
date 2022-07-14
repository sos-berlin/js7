package js7.data_for_java.command

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.command.CancellationMode
import js7.data.command.CancellationMode.Kill
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.workflow.position.JWorkflowPosition
import scala.jdk.OptionConverters.*

final case class JCancellationMode(asScala: CancellationMode)
extends JavaWrapper
{
  protected type AsScala = CancellationMode
}

object JCancellationMode
{
  val freshOnly = JCancellationMode(CancellationMode.FreshOnly)
  val freshOrStarted = JCancellationMode(CancellationMode.Default)

  /** Kill a running job (with SIGTERM if possible). */
  def kill: JCancellationMode =
    kill(immediately = false)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(immediately: Boolean): JCancellationMode =
    kill(immediately, Optional.empty)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(
    immediately: Boolean,
    @Nonnull position: Optional[JWorkflowPosition])
  : JCancellationMode =
    JCancellationMode(CancellationMode.FreshOrStarted(Some(
      Kill(immediately = immediately, position.toScala.map(_.asScala)))))
}
