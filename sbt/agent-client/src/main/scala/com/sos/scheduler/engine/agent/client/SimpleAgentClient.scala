package com.sos.scheduler.engine.agent.client

import akka.actor.ActorSystem
import com.sos.scheduler.engine.data.agent.AgentAddress
import spray.http.Uri

/**
 * Simple client for JobScheduler Agent.
 * <p>
 * Should be closed after use, to close all remaining HTTP connections.
 *
 * @author Joacim Zschimmer
 */
final class SimpleAgentClient private(val agentUri: Uri) extends AgentClient with AutoCloseable {

  protected val licenseKeys = Nil
  protected val actorRefFactory = ActorSystem("SimpleAgentClient")
  protected def hostConnectorSetupOption = None
  protected def userAndPasswordOption = None

  def close() = actorRefFactory.shutdown()

}

object SimpleAgentClient {
  def apply(agentUri: AgentAddress) = new SimpleAgentClient(agentUri.string)
}
