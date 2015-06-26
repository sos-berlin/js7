package com.sos.scheduler.engine.agent.tunnel

/**
 * @author Joacim Zschimmer
 */
private[agent] final case class DirectedRequest(tunnelIdWithPassword: TunnelId.WithPassword, request: Request)
