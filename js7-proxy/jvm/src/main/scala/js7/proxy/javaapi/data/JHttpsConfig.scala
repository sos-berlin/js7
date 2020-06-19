package js7.proxy.javaapi.data

import com.google.common.collect.{ImmutableCollection, ImmutableList}
import java.util.Optional
import js7.common.akkahttp.https.{HttpsConfig, KeyStoreRef, TrustStoreRef}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

final case class JHttpsConfig(
  keyStoreFile: Optional[KeyStoreRef],
  trustStoreRefs: ImmutableCollection[TrustStoreRef])
{
  def toScala = HttpsConfig(keyStoreFile.toScala, trustStoreRefs.asScala.toVector)
}

object JHttpsConfig
{
  val empty = JHttpsConfig(Optional.empty[KeyStoreRef], ImmutableList.of())
}
