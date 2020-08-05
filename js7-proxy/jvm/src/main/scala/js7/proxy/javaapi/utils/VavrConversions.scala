package js7.proxy.javaapi.utils

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.proxy.javaapi.utils.JavaUtils.Void

@javaApi
object VavrConversions
{
  implicit final class VavrOption[L, R](private val underlying: Either[L, R]) extends AnyVal
  {
    def toVavr: VEither[L, R] =
      underlying match {
        case Left(o) => VEither.left(o)
        case Right(o) => VEither.right(o)
      }

    def toVoidVavr: VEither[L, Void] =
      underlying.map(_ => Void).toVavr
  }
}
