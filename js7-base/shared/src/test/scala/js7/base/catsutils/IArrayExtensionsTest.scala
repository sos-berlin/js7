package js7.base.catsutils

import js7.base.catsutils.IArrayExtensions.*
import org.scalatest.freespec.AnyFreeSpec

final class IArrayExtensionsTest extends AnyFreeSpec:

  "mapOrKeep" in:
    val result = IArray(1, 2, 3).mapOrKeep:
      case 2 => 222
    val expected = IArray(1, 222, 3)
    assert(result.getClass == expected.getClass)
    assert(result.sameElements(expected))
