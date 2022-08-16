package js7.data_for_java.vavr

import io.vavr.control.Either as VEither
import js7.base.annotation.javaApi
import js7.base.test.Test
import js7.data_for_java.vavr.VavrConverters.*

/**
  * @author Joacim Zschimmer
  */
@javaApi
final class VavrConvertersTest extends Test
{
  "Either" in {
    assert(Left(7).toVavr == VEither.left(7))
    assert(Right(7).toVavr == VEither.right(7))
  }
}
