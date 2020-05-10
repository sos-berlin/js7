package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.tests.https.MasterClientSideHttpsWithoutCertificateTest._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.TimeoutException

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
    assert(!exception.isInstanceOf[TimeoutException])
    logger.info(exception.toString)  // Content of exception is not reliable. May be SSLxxException or TCP connection reset !!!
    //assert(exception.isInstanceOf[javax.net.ssl.SSLException] && exception.getMessage == "Received fatal alert: certificate_unknown" ||
    //       exception.toString == "javax.net.ssl.SSLHandshakeException: Received fatal alert: certificate_unknown]" ||  // Since Java 11
    //       exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }

  "Login" in {
    val exception = intercept[Exception] {
      masterApi.login() await 99.s
    }
    assert(!exception.isInstanceOf[TimeoutException])
    logger.info(exception.toString)  // Content of exception is not reliable. May be SSLxxException or TCP connection reset !!!
    //assert(exception.isInstanceOf[javax.net.ssl.SSLException] && exception.getMessage == "Received fatal alert: certificate_unknown" ||
    //       exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }
}

private object MasterClientSideHttpsWithoutCertificateTest
{
  private val logger = Logger(getClass)
}
