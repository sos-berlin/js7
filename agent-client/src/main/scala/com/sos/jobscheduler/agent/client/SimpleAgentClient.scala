package com.sos.jobscheduler.agent.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAny
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem

/**
 * Simple client for JobScheduler Agent Server.
 * <p>
 * Should be closed after use, to close all remaining HTTP connections.
 *
 * @author Joacim Zschimmer
 */
final class SimpleAgentClient(
  val baseUri: Uri,
  protected val keyStoreRef: Option[KeyStoreRef] = None,
  protected val trustStoreRef: Option[TrustStoreRef] = None)
extends HasCloser with AgentClient
{
  protected val name = "SimpleAgentClient"
  protected val actorSystem = newActorSystem("SimpleAgentClient") withCloser (_.terminate())

  onClose { super[AgentClient].close() }

  override def close() = super[HasCloser].close()
}
