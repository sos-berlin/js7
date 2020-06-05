package js7.data.job

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecutableTest extends AnyFreeSpec
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
    assert(ExecutablePath.checked("")             == Left(EmptyStringProblem("ExecutablePath")))
    assert(ExecutablePath.checked("/")            == Left(InvalidNameProblem("ExecutablePath", "/")))
    assert(ExecutablePath.checked("/.")           == Left(InvalidNameProblem("ExecutablePath", "/.")))
    assert(ExecutablePath.checked("/../file")     == Left(InvalidNameProblem("ExecutablePath", "/../file")))
    assert(ExecutablePath.checked("/./file")      == Left(InvalidNameProblem("ExecutablePath", "/./file")))
    assert(ExecutablePath.checked("/dir/./file")  == Left(InvalidNameProblem("ExecutablePath", "/dir/./file")))
    assert(ExecutablePath.checked("/")            == Left(InvalidNameProblem("ExecutablePath", "/")))
    assert(ExecutablePath.checked("file")         == Left(InvalidNameProblem("ExecutablePath", "file")))
    assert(ExecutablePath.checked("/.hidden")     == Left(InvalidNameProblem("ExecutablePath", "/.hidden")))
    assert(ExecutablePath.checked("""/a\b""")     == Left(InvalidNameProblem("ExecutablePath", """/a\b""")))
    assert(ExecutablePath.checked("/dir/../file") == Left(InvalidNameProblem("ExecutablePath", "/dir/../file")))
    assert(ExecutablePath.checked("/dir/./file")  == Left(InvalidNameProblem("ExecutablePath", "/dir/./file")))
  }

  "Valid ExecutablePath" in {
    assert(ExecutablePath.checked("/file")     == Right(ExecutablePath("/file")))
    assert(ExecutablePath.checked("/dir/file") == Right(ExecutablePath("/dir/file")))
    assert(ExecutablePath.checked("/file/")    == Right(ExecutablePath("/file/")))   // ?
  }
}
