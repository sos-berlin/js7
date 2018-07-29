package com.sos.jobscheduler.tests.https

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class AgentClientSideHttpsTest extends HttpsStandardTests
{
  override protected def agentHttpsMutual = true  // Agent requires client certificate from Master
  override protected def provideAgentClientCertificate = true
}
