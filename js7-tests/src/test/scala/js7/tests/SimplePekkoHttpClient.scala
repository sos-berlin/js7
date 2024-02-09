package js7.tests

import js7.base.io.https.HttpsConfig
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.http.PekkoHttpClient

/**
  * @author Joacim Zschimmer
  */
final class SimplePekkoHttpClient(
  label: String,
  protected val baseUri: Uri,
  protected val uriPrefixPath: String)
extends PekkoHttpClient:

  protected val name = label
  protected val httpsConfig = HttpsConfig.empty
  implicit val actorSystem = newActorSystem(label)

  protected def userAndPassword = None

  protected def sessionToken = None

  override def close() =
    super.close()
    Pekkos.terminateAndWait(actorSystem, 99.s)
