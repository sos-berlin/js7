package js7.common.http.configuration

import js7.base.time.ScalaTime._
import scala.concurrent.duration.FiniteDuration

final case class RecouplingStreamReaderConf(
  timeout: FiniteDuration,
  delay: FiniteDuration,
  failureDelay: FiniteDuration)

object RecouplingStreamReaderConf
{
  val forTest = RecouplingStreamReaderConf(55.s, 1.s, 5.s)
}
