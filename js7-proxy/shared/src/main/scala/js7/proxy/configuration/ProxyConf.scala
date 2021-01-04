package js7.proxy.configuration

import scala.concurrent.duration.FiniteDuration

final case class ProxyConf(
  tornOlder: Option[FiniteDuration],
  syncPolling: FiniteDuration)

object ProxyConf
{
  def default: ProxyConf = ProxyConfs.default
}
