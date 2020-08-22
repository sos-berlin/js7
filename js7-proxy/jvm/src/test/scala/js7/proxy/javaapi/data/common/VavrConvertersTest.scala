package js7.proxy.javaapi.data.common

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.proxy.javaapi.data.common.VavrConverters._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
@javaApi
final class VavrConvertersTest extends AnyFreeSpec
{
  "Either" in {
    assert(Left(7).toVavr == VEither.left(7))
    assert(Right(7).toVavr == VEither.right(7))
  }
}
