package com.sos.scheduler.engine.minicom.types

import java.util.UUID

/**
 * COM Class ID
 * @author Joacim Zschimmer
 */
final case class CLSID(uuid: UUID)

object CLSID {
  val Null = CLSID(new UUID(0, 0))
}
