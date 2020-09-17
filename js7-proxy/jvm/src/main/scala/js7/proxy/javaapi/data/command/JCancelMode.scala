package js7.proxy.javaapi.data.command

import js7.data.command.CancelMode
import js7.data.command.CancelMode.Kill
import js7.proxy.javaapi.data.common.JavaWrapper

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
  def kill(immediately: Boolean): JCancelMode =
    JCancelMode(CancelMode.FreshOrStarted(Some(Kill(immediately = immediately))))
}
