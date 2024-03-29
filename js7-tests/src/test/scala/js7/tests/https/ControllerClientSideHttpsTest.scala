package js7.tests.https

import javax.net.ssl.SSLException
import js7.base.io.https.HttpsConfig
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.controller.client.PekkoHttpControllerApi
import js7.tests.testenv.DirectoryProvider.ExportedControllerTrustStoreRef

/**
  * Controller and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
final class ControllerClientSideHttpsTest extends ControllerHttpsStandardTests:

  override protected def useCluster = false
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = true
  override protected def controllerHttpsMutual = true
  override protected def provideControllerClientCertificate = true

  "Client without HTTPS certificate is rejected" in:
    autoClosing(new PekkoHttpControllerApi(controller.localUri, None, actorSystem = actorSystem,
      httpsConfig = HttpsConfig(
        keyStoreRef = None,
        trustStoreRefs = ExportedControllerTrustStoreRef :: Nil))
    ) { api =>
      intercept[SSLException]:
        api.overview.await(99.s)
    }

  addTestsForCredentials()
