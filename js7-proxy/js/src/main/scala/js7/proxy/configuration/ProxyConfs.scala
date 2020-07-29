package js7.proxy.configuration

import js7.base.time.ScalaTime._

object ProxyConfs
{
  lazy val default = ProxyConf(
    tornOlder = Some(10.s))
}
