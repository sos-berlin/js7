package js7.data_for_java.command

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.command.SuspensionMode
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.workflow.position.JWorkflowPosition

@Deprecated
final case class JSuspendMode(asScala: SuspensionMode)
extends JavaWrapper
{
  protected type AsScala = SuspensionMode
}

object JSuspendMode
{
  /** PLEASE USE SAME METHOD IN JSuspensionMode. */
  @Deprecated
  def kill: JSuspensionMode =
    JSuspensionMode.kill(immediately = false)

  /** PLEASE USE SAME METHOD IN JSuspensionMode. */
  @Deprecated
  @Nonnull
  def kill(immediately: Boolean): JSuspensionMode =
    JSuspensionMode.kill(immediately, Optional.empty)

  /** PLEASE USE SAME METHOD IN JSuspensionMode. */
  @Deprecated
  @Nonnull
  def kill(
    immediately: Boolean,
    @Nonnull position: Optional[JWorkflowPosition]
  ): JSuspensionMode =
    JSuspensionMode.kill(immediately, position)
}
