package js7.common.scalautil

import js7.common.scalautil.GuavaUtils._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GuavaUtilsTest extends AnyFreeSpec
{
  "stringToInputStream" in {
    val in = stringToInputStream("heiß")
    assert(in.read() == 'h'.toByte)
    assert(in.read() == 'e'.toByte)
    assert(in.read() == 'i'.toByte)
    assert(in.read() == 0xC3)
    assert(in.read() == 0x9F)
    assert(in.read() == -1)
  }
}
