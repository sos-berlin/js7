package js7.proxy.configuration

import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.common.http.configuration.RecouplingStreamReaderConf

object ProxyConfs
{
  lazy val default = ProxyConf(
    tornOlder = Some(10.s),
    syncPolling = 100.ms,
    RecouplingStreamReaderConf(timeout = 55.s, delay = 1.s, failureDelays = Nel.one(3.s)))
}
