package com.sos.scheduler.engine.minicom.types

import java.util.UUID

/**
 * COM Interface ID
 * @author Joacim Zschimmer
 */
final case class IID(uuid: UUID) {
  override def toString = s"{$uuid}"
}

object IID {
  val Null = IID(new UUID(0, 0))
}
