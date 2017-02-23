package com.sos.jobscheduler.tunnel.data

import com.sos.jobscheduler.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class TunnelId(string: String) extends IsString {
  override def toString = s"tunnel/$string"  // Same as tunnel web service path
}

object TunnelId extends IsString.HasJsonFormat[TunnelId]
