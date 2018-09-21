package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class MasterClientSideHttpsWithoutCertificateTest extends HttpsTestBase
{
  override protected def masterHttpsMutual = true  // Master requires Clients certificate

  "overview" in {
    val exception = intercept[Exception] {
      masterApi.overview await 99.s
    }
    assert(exception.toString == "javax.net.ssl.SSLException: Received fatal alert: certificate_unknown" ||
           exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }

  "Login" in {
    val exception = intercept[Exception] {
      masterApi.login(Some(UserId("TEST-USER") → SecretString("TEST-PASSWORD"))) await 99.s
    }
    assert(exception.toString == "javax.net.ssl.SSLException: Received fatal alert: certificate_unknown" ||
           exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }
}
