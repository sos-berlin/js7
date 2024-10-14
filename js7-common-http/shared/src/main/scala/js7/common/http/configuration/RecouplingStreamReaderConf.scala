package js7.common.http.configuration

import js7.base.time.ScalaTime.*
import js7.base.utils.DelayConf
import scala.concurrent.duration.FiniteDuration

final case class RecouplingStreamReaderConf(
  timeout: Option[FiniteDuration],
  keepAlive: FiniteDuration,
  delay: FiniteDuration,
  delayConf: DelayConf)


object RecouplingStreamReaderConf:
  val forTest: RecouplingStreamReaderConf =
    RecouplingStreamReaderConf(Some(55.s), 1.s, 1.s, DelayConf(5.s))
