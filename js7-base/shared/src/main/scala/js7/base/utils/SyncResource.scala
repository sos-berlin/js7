package js7.base.utils

import cats.effect.{Resource, SyncIO}
import izumi.reflect.Tag
import java.io.{ByteArrayInputStream, InputStream}
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.utils.CatsUtils.syntax.RichResource

/**
  * @author Joacim Zschimmer
  */
object SyncResource:
  def byteArrayToResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(
      SyncIO[InputStream] {
        new ByteArrayInputStream(bytes)
      })

  object syntax:
    implicit final class RichSyncResource[A](private val underlying: Resource[SyncIO, A])
    extends AnyVal:
      /** Like `SyncIO`'s `use`, but synchronously. */
      def useSync[B](f: A => B)(using Tag[A]): B =
        underlying.toAllocated[SyncIO, A].run().blockingUse(f)

    implicit final class ByteArrayAsResource[A](private val underlying: Array[Byte]) extends AnyVal:
      def asResource: Resource[SyncIO, InputStream] = byteArrayToResource(underlying)
