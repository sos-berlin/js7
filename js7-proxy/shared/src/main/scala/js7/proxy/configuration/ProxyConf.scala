package js7.proxy.configuration

import js7.common.http.configuration.RecouplingStreamReaderConf
import scala.concurrent.duration.FiniteDuration

final case class ProxyConf(
  tornOlder: Option[FiniteDuration],
  syncPolling: FiniteDuration,
  recouplingStreamReaderConf: RecouplingStreamReaderConf)

object ProxyConf:
  def default: ProxyConf = ProxyConfs.default
