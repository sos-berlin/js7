package js7.base.utils

import cats.effect.{Bracket, Resource, SyncIO}
import java.io.{ByteArrayInputStream, InputStream}

/**
  * @author Joacim Zschimmer
  */
object SyncResource
{
  def byteArrayToResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(
      SyncIO[InputStream] {
        new ByteArrayInputStream(bytes)
      })

  object syntax {
    implicit final class RichSyncResource[A](private val underlying: Resource[SyncIO, A]) extends AnyVal {
      /** Like `SyncIO`'s `use`, but synchronously. */
      def useSync[B](f: A => B)(implicit F: Bracket[SyncIO, Throwable]): B =
        underlying.use(a =>
          SyncIO {
            f(a)
          }
        ).unsafeRunSync()  // SyncIO's unsafeRunSync does not block
    }

    implicit final class ByteArrayAsResource[A](private val underlying: Array[Byte]) extends AnyVal {
      def asResource: Resource[SyncIO, InputStream] = byteArrayToResource(underlying)
    }
  }
}
