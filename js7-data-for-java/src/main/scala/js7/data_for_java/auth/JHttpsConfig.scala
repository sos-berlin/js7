package js7.data_for_java.auth

import java.util.Optional
import javax.annotation.Nonnull
import js7.base.io.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import js7.data_for_java.common.JavaWrapper
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JHttpsConfig(asScala: HttpsConfig) extends JavaWrapper
{
  type AsScala = HttpsConfig
}

object JHttpsConfig
{
  val empty = JHttpsConfig(HttpsConfig.empty)

  @Nonnull
  def of(
    @Nonnull keyStoreFile: Optional[KeyStoreRef],
    @Nonnull trustStoreRefs: java.util.List[TrustStoreRef])
  : HttpsConfig =
    HttpsConfig(keyStoreFile.toScala, trustStoreRefs.asScala.toSeq)
}
