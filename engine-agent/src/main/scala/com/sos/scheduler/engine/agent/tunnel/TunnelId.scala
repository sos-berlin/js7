package com.sos.scheduler.engine.agent.tunnel

import com.sos.scheduler.engine.data.base.IsString

/**
 * @author Joacim Zschimmer
 */
final case class TunnelId(string: String) extends IsString

object TunnelId extends IsString.HasJsonFormat[TunnelId]
