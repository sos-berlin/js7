package com.sos.jobscheduler.data.job

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import org.scalatest.FreeSpec
import com.sos.jobscheduler.tester.CirceJsonTester._
import com.sos.jobscheduler.base.circeutils.CirceUtils._

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
    assert(ExecutablePath.checked("")             == Invalid(Problem("Executable path must not be empty")))
    assert(ExecutablePath.checked("/")            == Invalid(Problem("Invalid executable path: /")))
    assert(ExecutablePath.checked("/.")           == Invalid(Problem("Invalid executable path: /.")))
    assert(ExecutablePath.checked("/../file")     == Invalid(Problem("Invalid executable path: /../file")))
    assert(ExecutablePath.checked("/./file")      == Invalid(Problem("Invalid executable path: /./file")))
    assert(ExecutablePath.checked("/dir/./file")  == Invalid(Problem("Invalid executable path: /dir/./file")))
    assert(ExecutablePath.checked("/")            == Invalid(Problem("Invalid executable path: /")))
    assert(ExecutablePath.checked("file")         == Invalid(Problem("Invalid executable path: file")))
    assert(ExecutablePath.checked("/.hidden")     == Invalid(Problem("Invalid executable path: /.hidden")))
    assert(ExecutablePath.checked("""/a\b""")     == Invalid(Problem("""Invalid executable path: /a\b""")))
    assert(ExecutablePath.checked("/dir/../file") == Invalid(Problem("Invalid executable path: /dir/../file")))
    assert(ExecutablePath.checked("/dir/./file")  == Invalid(Problem("Invalid executable path: /dir/./file")))
  }

  "Valid ExecutablePath" in {
    assert(ExecutablePath.checked("/file")     == Valid(ExecutablePath("/file")))
    assert(ExecutablePath.checked("/dir/file") == Valid(ExecutablePath("/dir/file")))
    assert(ExecutablePath.checked("/file/")    == Valid(ExecutablePath("/file/")))   // ?
  }
}
