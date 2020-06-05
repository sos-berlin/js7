package js7.proxy.javaapi.utils

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.proxy.javaapi.utils.VavrConversions._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
@javaApi
final class VavrConversionsTest extends AnyFreeSpec
{
  "Either" in {
    assert(Left(7).asVavr == VEither.left(7))
    assert(Right(7).asVavr == VEither.right(7))
  }
}
