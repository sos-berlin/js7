package com.sos.scheduler.engine.minicom.types

import java.util.UUID

/**
 * COM Interface ID
 * @author Joacim Zschimmer
 */
final case class IID(uuid: UUID)

object IID {
  val Null = IID(new UUID(0, 0))
}
