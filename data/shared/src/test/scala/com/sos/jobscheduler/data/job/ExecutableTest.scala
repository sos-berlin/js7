package com.sos.jobscheduler.data.job

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.GenericString.EmptyStringProblem
import com.sos.jobscheduler.base.problem.Problems.InvalidNameProblem
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecutableTest extends FreeSpec
{
  "JSON" - {
    "ExecutablePath" in {
      testJson[Executable](ExecutablePath("/EXECUTABLE"), json"""
        {
          "TYPE": "ExecutablePath",
          "path": "/EXECUTABLE"
        }
      """)
    }

    "ExecutableScript" in {
      testJson[Executable](ExecutableScript("SCRIPT"), json"""
        {
          "TYPE": "ExecutableScript",
          "script": "SCRIPT"
        }
      """)
    }
  }

  "Invalid ExecutablePath" in {
    assert(ExecutablePath.checked("")             == Invalid(EmptyStringProblem("ExecutablePath")))
    assert(ExecutablePath.checked("/")            == Invalid(InvalidNameProblem("ExecutablePath", "/")))
    assert(ExecutablePath.checked("/.")           == Invalid(InvalidNameProblem("ExecutablePath", "/.")))
    assert(ExecutablePath.checked("/../file")     == Invalid(InvalidNameProblem("ExecutablePath", "/../file")))
    assert(ExecutablePath.checked("/./file")      == Invalid(InvalidNameProblem("ExecutablePath", "/./file")))
    assert(ExecutablePath.checked("/dir/./file")  == Invalid(InvalidNameProblem("ExecutablePath", "/dir/./file")))
    assert(ExecutablePath.checked("/")            == Invalid(InvalidNameProblem("ExecutablePath", "/")))
    assert(ExecutablePath.checked("file")         == Invalid(InvalidNameProblem("ExecutablePath", "file")))
    assert(ExecutablePath.checked("/.hidden")     == Invalid(InvalidNameProblem("ExecutablePath", "/.hidden")))
    assert(ExecutablePath.checked("""/a\b""")     == Invalid(InvalidNameProblem("ExecutablePath", """/a\b""")))
    assert(ExecutablePath.checked("/dir/../file") == Invalid(InvalidNameProblem("ExecutablePath", "/dir/../file")))
    assert(ExecutablePath.checked("/dir/./file")  == Invalid(InvalidNameProblem("ExecutablePath", "/dir/./file")))
  }

  "Valid ExecutablePath" in {
    assert(ExecutablePath.checked("/file")     == Valid(ExecutablePath("/file")))
    assert(ExecutablePath.checked("/dir/file") == Valid(ExecutablePath("/dir/file")))
    assert(ExecutablePath.checked("/file/")    == Valid(ExecutablePath("/file/")))   // ?
  }
}
