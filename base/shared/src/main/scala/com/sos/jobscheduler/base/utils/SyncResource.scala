package com.sos.jobscheduler.base.utils

import cats.effect.{Bracket, Resource, SyncIO}
import java.io.{ByteArrayInputStream, InputStream}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object SyncResource
{
  def byteArrayToResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO[InputStream] { new ByteArrayInputStream(bytes) })

  object ops {
    implicit final class RichResource[A](private val underlying: Resource[SyncIO, A]) extends AnyVal {
      /** Like `SyncIO`'s `use`, but synchronously. */
      def useSync[B](f: A ⇒ B)(implicit F: Bracket[SyncIO, Throwable]): B =
        underlying.use(a ⇒
          SyncIO {
            f(a)
          }
        ).unsafeRunSync()  // SyncIO's unsafeRunSync does not block (despite its documentation)
    }

    implicit final class ByteArrayAsResource[A](private val underlying: Array[Byte]) extends AnyVal {
      def asResource: Resource[SyncIO, InputStream] = byteArrayToResource(underlying)
    }
  }
}
