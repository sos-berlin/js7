package com.sos.jobscheduler.tests.https

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class MasterClientSideHttpsTest extends HttpsStandardTests
{
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = true
  override protected def masterHttpsMutual = true
  override protected def provideMasterClientCertificate = true
}
