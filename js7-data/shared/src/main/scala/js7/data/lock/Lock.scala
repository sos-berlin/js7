package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.Assertions.assertThat

final case class Lock(id: LockId, limit: Int) {

  assertThat(limit >= 0)
}

object Lock {
  implicit val jsonCodec = deriveCodec[Lock]
}
