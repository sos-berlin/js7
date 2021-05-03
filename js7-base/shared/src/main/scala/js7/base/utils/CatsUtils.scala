package js7.base.utils

import cats.data.Validated
import cats.effect.{Resource, SyncIO}
import cats.kernel.Monoid
import cats.syntax.foldable._
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import js7.base.problem.Problem
import js7.base.utils.StackTraces._

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  def combine[A: Monoid](as: A*): A =
    as.combineAll

  private def bytesToInputStreamResource(bytes: collection.Seq[Byte]): Resource[SyncIO, InputStream] =
    bytesToInputStreamResource(bytes.toArray)

  private def bytesToInputStreamResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { new ByteArrayInputStream(bytes): InputStream })

  private def base64ToStreamResource(base64: String): Resource[SyncIO, InputStream] =
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
