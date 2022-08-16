package js7.data.job

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.ReturnCode
import js7.base.test.Test
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ReturnCodeMeaningTest extends Test
{
  "Success" in {
    val success0 = ReturnCodeMeaning.Success(Set(ReturnCode(0)))
    assert(success0.isSuccess(ReturnCode(0)))
    assert(!success0.isSuccess(ReturnCode(1)))

    val success13 = ReturnCodeMeaning.Success(Set(ReturnCode(1), ReturnCode(3)))
    assert(!success13.isSuccess(ReturnCode(0)))
    assert(success13.isSuccess(ReturnCode(1)))
    assert(success13.isSuccess(ReturnCode(3)))

    val noSuccess = ReturnCodeMeaning.Success(Set.empty)
    assert(!noSuccess.isSuccess(ReturnCode(0)))
    assert(!noSuccess.isSuccess(ReturnCode(1)))
  }

  "Failure" in {
    val failure0 = ReturnCodeMeaning.Failure(Set(ReturnCode(0)))
    assert(!failure0.isSuccess(ReturnCode(0)))
    assert(failure0.isSuccess(ReturnCode(1)))

    val failure13 = ReturnCodeMeaning.Failure(Set(ReturnCode(1), ReturnCode(3)))
    assert(failure13.isSuccess(ReturnCode(0)))
    assert(!failure13.isSuccess(ReturnCode(1)))
    assert(!failure13.isSuccess(ReturnCode(3)))

    val noFailure = ReturnCodeMeaning.Failure(Set.empty)
    assert(noFailure.isSuccess(ReturnCode(0)))
    assert(noFailure.isSuccess(ReturnCode(1)))
  }

  "JSON" in {
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Default,
      json"""{
        "success": [ 0 ]
      }""")
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Success(Set.empty),
      json"""{
        "success": []
      }""")
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Success(Set(ReturnCode(0), ReturnCode(3))),
      json"""{
        "success": [ 0, 3 ]
      }""")
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Failure(Set.empty),
      json"""{
        "failure": []
      }""")
    testJson[ReturnCodeMeaning](ReturnCodeMeaning.Failure(Set(ReturnCode(1), ReturnCode(3))),
      json"""{
        "failure": [ 1, 3 ]
      }""")
  }
}
