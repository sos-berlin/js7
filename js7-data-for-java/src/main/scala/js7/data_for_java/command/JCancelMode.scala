package  js7.data_for_java.command

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.command.CancelMode
import js7.data.command.CancelMode.Kill
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.workflow.position.JWorkflowPosition
import scala.jdk.OptionConverters._

final case class JCancelMode(asScala: CancelMode)
extends JavaWrapper
{
  protected type AsScala = CancelMode
}

object JCancelMode
{
  val freshOnly = JCancelMode(CancelMode.FreshOnly)
  val freshOrStarted = JCancelMode(CancelMode.Default)

  /** Kill a running job (with SIGTERM if possible). */
  def kill: JCancelMode =
    kill(immediately = false)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(immediately: Boolean): JCancelMode =
    kill(immediately, Optional.empty)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(
    immediately: Boolean,
    @Nonnull position: Optional[JWorkflowPosition])
  : JCancelMode =
    JCancelMode(CancelMode.FreshOrStarted(Some(
      Kill(immediately = immediately, position.toScala.map(_.asScala)))))
}
