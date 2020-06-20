package js7.tests

import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.AkkaHttpClient
import js7.common.scalautil.Futures.implicits._

/**
  * @author Joacim Zschimmer
  */
final class SimpleAkkaHttpClient(
  label: String,
  protected val baseUri: Uri,
  protected val uriPrefixPath: String) extends AkkaHttpClient
{
  protected val name = label
  protected def keyStoreRef = None
  protected def trustStoreRefs = Nil
  implicit val actorSystem = newActorSystem(label)

  protected def userAndPassword = None

  protected def sessionToken = None

  override def close() = {
    super.close()
    actorSystem.terminate() await 99.s
  }
}
