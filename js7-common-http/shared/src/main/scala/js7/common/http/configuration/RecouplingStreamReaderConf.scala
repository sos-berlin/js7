package js7.common.http.configuration

import scala.concurrent.duration.FiniteDuration

final case class RecouplingStreamReaderConf(
  timeout: FiniteDuration,
  delay: FiniteDuration,
  failureDelay: FiniteDuration)
