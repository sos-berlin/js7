package js7.tests.https

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class AgentClientSideHttpsTest extends MasterHttpsStandardTests
{
  override protected def agentHttpsMutual = true  // Agent requires client certificate from Master
  override protected def provideAgentClientCertificate = true
}
