package com.sos.scheduler.engine.tunnel.data

import com.sos.scheduler.engine.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class TunnelId(string: String) extends IsString {
  override def toString = s"tunnel/$string"  // Same as tunnel web service path
}

object TunnelId extends IsString.HasJsonFormat[TunnelId]
