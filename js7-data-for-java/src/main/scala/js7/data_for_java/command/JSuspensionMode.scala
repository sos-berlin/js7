package js7.data_for_java.command

import java.util.Optional
import javax.annotation.Nonnull
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.CancellationMode.Kill
import js7.data.command.SuspensionMode
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.workflow.position.JWorkflowPosition
import scala.jdk.OptionConverters.*

final case class JSuspensionMode(asScala: SuspensionMode)
extends JavaWrapper:

  type AsScala = SuspensionMode


object JSuspensionMode:
  @Nonnull
  val standard: JSuspensionMode =
    JSuspensionMode(SuspensionMode.standard)

  def of(
    resetState: Boolean,
    kill: Boolean,
    killImmediately: Boolean,
    position: Optional[JWorkflowPosition])
  : JSuspensionMode =
    JSuspensionMode(SuspensionMode(
      resetState = resetState,
      kill = kill || killImmediately thenSome:
        Kill(
          immediately = killImmediately,
          workflowPosition = position.map(_.asScala).toScala)))

  /** Kill a running job (with SIGTERM if possible). */
  @Nonnull
  val kill: JSuspensionMode =
    kill(immediately = false)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(immediately: Boolean): JSuspensionMode =
    kill(immediately, Optional.empty)

  /** Kill a running job.
    * @param immediately true: try SIGKILL else SIGTERM
    **/
  @Nonnull
  def kill(
    immediately: Boolean,
    @Nonnull position: Optional[JWorkflowPosition])
  : JSuspensionMode =
    JSuspensionMode(SuspensionMode(
      kill = Some(Kill(
        immediately = immediately,
        position.toScala.map(_.asScala)))))
