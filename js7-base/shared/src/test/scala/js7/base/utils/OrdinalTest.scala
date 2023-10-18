package js7.base.utils

import js7.base.generic.GenericInt
import js7.base.test.OurTestSuite
import js7.base.utils.Ordinal.syntax.*
import js7.base.utils.OrdinalTest.*

final class OrdinalTest extends OurTestSuite:
  "Int" in:
    assert(!1.isSuccessorOf(1))
    assert(!3.isSuccessorOf(1))
    assert(1 isSuccessorOf 0)

    assert(Int.MinValue == Int.MaxValue + 1)
    assert(Int.MinValue.isSuccessorOf(Int.MaxValue))

  "Char" in:
    assert(!'a'.isSuccessorOf('a'))
    assert('b'.isSuccessorOf('a'))

    assert('\u0000' == ('\uffff' + 1).toChar)
    assert(!'\u0000'.isSuccessorOf('\uffff'))

  "GenericInt" in:
    assert(!Test(1).isSuccessorOf(Test(1)))
    assert(!Test(3).isSuccessorOf(Test(1)))
    assert(Test(1) isSuccessorOf Test(0))

    assert(Test(Int.MinValue) == Test(Int.MaxValue + 1))
    assert(Test(Int.MinValue).isSuccessorOf(Test(Int.MaxValue)))


object OrdinalTest:
  private final case class Test(number: Int) extends GenericInt
  private object Test extends GenericInt.Companion[Test]
