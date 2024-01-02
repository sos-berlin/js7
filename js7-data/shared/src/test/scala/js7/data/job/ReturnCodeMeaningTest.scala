package js7.data.job

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.ReturnCode
import js7.base.test.OurTestSuite
import js7.base.utils.RangeSet
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ReturnCodeMeaningTest extends OurTestSuite:
  "Success" in:
    val success0 = ReturnCodeMeaning.Success(RangeSet(ReturnCode(0)))
    assert(success0.isSuccess(ReturnCode(0)))
    assert(!success0.isSuccess(ReturnCode(1)))

    val success13 = ReturnCodeMeaning.Success(RangeSet(ReturnCode(1), ReturnCode(3)))
    assert(!success13.isSuccess(ReturnCode(0)))
    assert(success13.isSuccess(ReturnCode(1)))
    assert(success13.isSuccess(ReturnCode(3)))

    val noSuccess = ReturnCodeMeaning.Success(RangeSet.empty)
    assert(!noSuccess.isSuccess(ReturnCode(0)))
    assert(!noSuccess.isSuccess(ReturnCode(1)))

  "Failure" in:
    val failure0 = ReturnCodeMeaning.Failure(RangeSet(ReturnCode(0)))
    assert(!failure0.isSuccess(ReturnCode(0)))
    assert(failure0.isSuccess(ReturnCode(1)))

    val failure13 = ReturnCodeMeaning.Failure(RangeSet(ReturnCode(1), ReturnCode(3)))
    assert(failure13.isSuccess(ReturnCode(0)))
    assert(!failure13.isSuccess(ReturnCode(1)))
    assert(!failure13.isSuccess(ReturnCode(3)))

    val noFailure = ReturnCodeMeaning.Failure(RangeSet.empty)
    assert(noFailure.isSuccess(ReturnCode(0)))
    assert(noFailure.isSuccess(ReturnCode(1)))

  "JSON" in:
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Default,
      json"""{ "success": "0" }""",
      json"""{ "success": [ 0 ] }""")

    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Success(RangeSet.empty),
      json"""{ "success": "" }""",
      json"""{ "success": [] }""")

    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Success(RangeSet(ReturnCode(0), ReturnCode(3))),
      json"""{ "success": "0,3" }""",
      json"""{ "success": [ 0, 3 ] }""")

    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Success(
      RangeSet.fromRanges(Seq(
        RangeSet.Single(ReturnCode(1)),
        RangeSet.Interval(ReturnCode(3), ReturnCode(9))))),
      json"""{ "success":  "1,3..9" }""",
      json"""{ "success": [ 1, [3, 9] ] }""")

    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Failure(RangeSet.empty),
      json"""{ "failure": "" }""",
      json"""{ "failure": [] }""")
