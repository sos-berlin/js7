package js7.base.utils

import cats.syntax.traverse.*
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*

final class CatsTest extends OurTestSuite:

  "sequence" in:
    val list = List(Left(1), Right(2), Left(3), Right(4))
    assert(list.sequence == Left(1))
