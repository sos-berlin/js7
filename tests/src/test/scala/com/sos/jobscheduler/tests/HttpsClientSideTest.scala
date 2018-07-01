package com.sos.jobscheduler.tests

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class HttpsClientSideTest extends HttpsServerSideTest
{
  override protected def agentHttpsMutual = true

  // FIXME Test for masterApi->Master needs own (self-signed) certificate: override protected def masterHttpsMutual = true
}
