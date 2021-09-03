package js7.base.number

import js7.base.number.Numbers.{addSaturating, subtractSaturating}
import org.scalatest.freespec.AnyFreeSpec

final class NumbersTest extends AnyFreeSpec
{
  "addSaturating" in {
    assert(addSaturating(Long.MaxValue, 1) == Long.MaxValue)
    assert(addSaturating(Long.MaxValue, Long.MaxValue) == Long.MaxValue)
    assert(addSaturating(Long.MaxValue - 2, 3) == Long.MaxValue)
    assert(addSaturating(Long.MaxValue - 2, 2) == Long.MaxValue)
    assert(addSaturating(Long.MaxValue - 2, 1) == Long.MaxValue - 1)

    assert(addSaturating(Long.MinValue, Long.MinValue) == Long.MinValue)
    assert(addSaturating(Long.MinValue + 2, -3) == Long.MinValue)
    assert(addSaturating(Long.MinValue + 2, -2) == Long.MinValue)
    assert(addSaturating(Long.MinValue + 2, -1) == Long.MinValue + 1)

    assert(addSaturating(10, 1) == 11)
    assert(addSaturating(10, -1) == 9)
    assert(addSaturating(-10, 1) == -9)
    assert(addSaturating(-10, -1) == -11)
  }

  "subtractSaturating" in {
    assert(subtractSaturating(Long.MaxValue, -Long.MaxValue) == Long.MaxValue)
    assert(subtractSaturating(Long.MaxValue - 2, -3) == Long.MaxValue)
    assert(subtractSaturating(Long.MaxValue - 2, -2) == Long.MaxValue)
    assert(subtractSaturating(Long.MaxValue - 2, -1) == Long.MaxValue - 1)

    assert(subtractSaturating(Long.MinValue, Long.MinValue) == 0)
    assert(subtractSaturating(Long.MinValue + 2, 3) == Long.MinValue)
    assert(subtractSaturating(Long.MinValue + 2, 2) == Long.MinValue)
    assert(subtractSaturating(Long.MinValue + 2, 1) == Long.MinValue + 1)

    assert(subtractSaturating(10, 1) == 9)
    assert(subtractSaturating(10, -1) == 11)
    assert(subtractSaturating(-10, 1) == -11)
    assert(subtractSaturating(-10, -1) == -9)
  }
}
