package js7.tests.https

import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.tests.https.ControllerClientSideHttpsWithoutCertificateTest._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.TimeoutException

/**
  * @author Joacim Zschimmer
  */
final class ControllerClientSideHttpsWithoutCertificateTest extends HttpsTestBase
{
  override protected def controllerHttpsMutual = true  // Controller requires Clients certificate
  override protected def useCluster = false

  "overview" in {
    val exception = intercept[Exception] {
      controllerApi.overview await 99.s
    }
    assert(!exception.isInstanceOf[TimeoutException])
    logger.info(exception.toString)  // Content of exception is not reliable. May be SSLxxException or TCP connection reset !!!
    //assert(exception.isInstanceOf[javax.net.ssl.SSLException] && exception.getMessage == "Received fatal alert: certificate_unknown" ||
    //       exception.toString == "javax.net.ssl.SSLHandshakeException: Received fatal alert: certificate_unknown]" ||  // Since Java 11
    //       exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }

  "Login" in {
    val exception = intercept[Exception] {
      controllerApi.login() await 99.s
    }
    assert(!exception.isInstanceOf[TimeoutException])
    logger.info(exception.toString)  // Content of exception is not reliable. May be SSLxxException or TCP connection reset !!!
    //assert(exception.isInstanceOf[javax.net.ssl.SSLException] && exception.getMessage == "Received fatal alert: certificate_unknown" ||
    //       exception.toString == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }
}

private object ControllerClientSideHttpsWithoutCertificateTest
{
  private val logger = Logger(getClass)
}
