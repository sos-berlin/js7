package js7.proxy.javaapi.data.auth

import java.util.Optional
import js7.common.akkahttp.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.proxy.javaapi.data.common.JavaWrapper
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

final case class JHttpsConfig(underlying: HttpsConfig) extends JavaWrapper
{
  protected type Underlying = HttpsConfig
}

object JHttpsConfig
{
  val empty = JHttpsConfig(HttpsConfig.empty)

  def of(keyStoreFile: Optional[KeyStoreRef], trustStoreRefs: java.util.List[TrustStoreRef]) =
    HttpsConfig(keyStoreFile.toScala, trustStoreRefs.asScala.toSeq)
}
