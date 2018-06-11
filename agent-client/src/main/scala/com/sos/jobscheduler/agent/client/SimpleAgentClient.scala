package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.HasCloser

/**
 * Simple client for JobScheduler Agent.
 * <p>
 * Should be closed after use, to close all remaining HTTP connections.
 *
 * @author Joacim Zschimmer
 */
final class SimpleAgentClient(val baseUri: Uri)
extends HasCloser with AgentClient {

  protected val actorSystem = newActorSystem("SimpleAgentClient") withCloser (_.terminate())

  onClose { super[AgentClient].close() }

  override def close() = super[HasCloser].close()
}
