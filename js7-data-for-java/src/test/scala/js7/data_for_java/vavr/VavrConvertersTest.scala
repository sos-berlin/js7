package js7.data_for_java.vavr

import io.vavr.control.Either as VEither
import js7.base.annotation.javaApi
import js7.data_for_java.vavr.VavrConverters.*
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
