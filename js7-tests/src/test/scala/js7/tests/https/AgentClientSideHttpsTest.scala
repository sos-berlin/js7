package js7.tests.https

/**
  * Controller and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class AgentClientSideHttpsTest extends ControllerHttpsStandardTests:
  override protected def agentHttpsMutual = true  // Agent requires client certificate from Controller
  override protected def provideAgentClientCertificate = true

  addTestsForCredentials()
