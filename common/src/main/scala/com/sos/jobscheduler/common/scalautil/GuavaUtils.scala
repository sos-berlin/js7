package js7.common.scalautil

import cats.effect.{Resource, SyncIO}
import com.google.common.io.CharSource
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8

/**
  * @author Joacim Zschimmer
  */
object GuavaUtils
{
  def stringToInputStreamResource(string: String): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { stringToInputStream(string) })

  def stringToInputStream(string: String): InputStream =
    CharSource.wrap(string).asByteSource(UTF_8).openStream()
}
