package js7.agent.client

import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.*
import js7.base.io.https.HttpsConfig
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem

/**
 * Simple client for JS7 Agent.
 * <p>
 * Should be closed after use, to close all remaining HTTP connections.
 *
 * @author Joacim Zschimmer
 */
final class SimpleAgentClient(
  val baseUri: Uri,
  protected val userAndPassword: Option[UserAndPassword],
  protected val httpsConfig: HttpsConfig = HttpsConfig.empty)
extends HasCloser with AgentClient:

  protected val name = "SimpleAgentClient"
  protected val actorSystem =
    newActorSystem("SimpleAgentClient", config"pekko.log-dead-letters = 0")
      .withCloser(Pekkos.terminateAndWait(_, 10.s/*!!!*/))

  onClose { super[AgentClient].close() }

  override def close() =
    logOpenSession()
    super[HasCloser].close()

  protected val prefixedUri = baseUri / "agent"

  override def toString = s"SimpleAgentClient($prefixedUri)"
