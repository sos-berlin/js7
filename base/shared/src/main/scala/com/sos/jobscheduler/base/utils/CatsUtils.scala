package js7.base.utils

import cats.data.Validated
import cats.effect.{Resource, SyncIO}
import js7.base.problem.Problem
import js7.base.utils.StackTraces._
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  def bytesToInputStreamResource(bytes: collection.Seq[Byte]): Resource[SyncIO, InputStream] =
    bytesToInputStreamResource(bytes.toArray)

  def bytesToInputStreamResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { new ByteArrayInputStream(bytes): InputStream })

  def base64ToStreamResource(base64: String): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO[InputStream] {
      try new ByteArrayInputStream(Base64.getMimeDecoder.decode(base64))
      catch { case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Error in Base64 encoded data: ${e.getMessage}", e)
      }
    })

  implicit final class RichThrowableValidated[E <: Throwable, A](private val underlying: Validated[E, A]) extends AnyVal
  {
    def orThrow: A =
      underlying.valueOr(t => throw t.appendCurrentStackTrace)
  }

  implicit final class RichProblemValidated[E <: Problem, A](private val underlying: Validated[E, A]) extends AnyVal
  {
    def orThrow: A =
      underlying.valueOr(problem => throw problem.throwable.appendCurrentStackTrace)
  }
}
