package js7.common.akkahttp.https

import scala.collection.immutable.Seq

final case class HttpsConfig(
  keyStoreRef: Option[KeyStoreRef],
  trustStoreRefs: Seq[TrustStoreRef])

object HttpsConfig
{
  val empty = HttpsConfig(None, Nil)
}
