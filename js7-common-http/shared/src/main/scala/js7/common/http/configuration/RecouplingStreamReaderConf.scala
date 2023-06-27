package js7.common.http.configuration

import cats.data.NonEmptyList
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import scala.concurrent.duration.FiniteDuration

final case class RecouplingStreamReaderConf(
  timeout: FiniteDuration,
  delay: FiniteDuration,
  failureDelays: NonEmptyList[FiniteDuration])

object RecouplingStreamReaderConf
{
  val forTest = RecouplingStreamReaderConf(55.s, 1.s, Nel.one(5.s))
}
