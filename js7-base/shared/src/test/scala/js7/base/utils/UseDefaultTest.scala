package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.UseDefault.*

final class UseDefaultTest extends OurTestSuite:

  "getOrElse" in:
    val a: Int | UseDefault = 7
    assert(a.getOrElse(0) == 7)

    val useDefault: Int | UseDefault = UseDefault
    assert(useDefault.getOrElse(0) == 0)

  "toOption" in:
    val a: Int | UseDefault = 7
    assert(a.toOption == Some(7))

    val useDefault: Int | UseDefault = UseDefault
    assert(useDefault.toOption == None)
