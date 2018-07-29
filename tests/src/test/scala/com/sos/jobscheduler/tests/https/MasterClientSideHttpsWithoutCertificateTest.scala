package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class MasterClientSideHttpsWithoutCertificateTest extends HttpsTestBase
{
  override protected def masterHttpsMutual = true  // Master requires Clients certificate

  "overview" in {
    intercept[javax.net.ssl.SSLException] {
      masterApi.overview await 99.s
    } .getMessage shouldEqual "Received fatal alert: certificate_unknown"
  }

  "Login" in {
    intercept[javax.net.ssl.SSLException] {
      masterApi.login(Some(UserId("TEST-USER") → SecretString("TEST-PASSWORD"))) await 99.s
    } .getMessage shouldEqual "Received fatal alert: certificate_unknown"
  }
}
