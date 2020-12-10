package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.Assertions.assertThat

final case class Lock(id: LockId, nonExclusiveLimit: Option[Int] = None) {
  for (nonExclusiveLimit <- nonExclusiveLimit) assertThat(nonExclusiveLimit > 0)

  def limit = nonExclusiveLimit getOrElse 1
}

object Lock {
  implicit val jsonCodec = deriveCodec[Lock]
}
