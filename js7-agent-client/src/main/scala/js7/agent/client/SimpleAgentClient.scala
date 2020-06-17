package js7.agent.client

import js7.base.auth.UserAndPassword
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.Akkas.newActorSystem

/**
 * Simple client for JS7 Agent Server.
 * <p>
 * Should be closed after use, to close all remaining HTTP connections.
 *
 * @author Joacim Zschimmer
 */
final class SimpleAgentClient(
  val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val keyStoreRef: Option[KeyStoreRef] = None,
  protected val trustStoreRefs: Seq[TrustStoreRef] = Nil)
extends HasCloser with AgentClient
{
  protected val name = "SimpleAgentClient"
  protected val actorSystem = newActorSystem("SimpleAgentClient") withCloser (_.terminate())

  onClose { super[AgentClient].close() }

  override def close() = super[HasCloser].close()
}
