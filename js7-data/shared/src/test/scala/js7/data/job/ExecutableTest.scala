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
    "RelativeExecutablePath" in {
      testJson[Executable](RelativeExecutablePath("EXECUTABLE"), json"""
        {
          "TYPE": "ExecutablePath",
          "path": "EXECUTABLE"
        }
      """)
    }

    "AbsoluteExecutablePath" in {
      testJson[Executable](AbsoluteExecutablePath("/EXECUTABLE"), json"""
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
    assert(ExecutablePath.checked("")            == Left(EmptyStringProblem("RelativeExecutablePath")))
    assert(ExecutablePath.checked(" ")           == Left(InvalidNameProblem("RelativeExecutablePath", " ")))
    assert(ExecutablePath.checked(" X")          == Left(InvalidNameProblem("RelativeExecutablePath", " X")))
    assert(ExecutablePath.checked("\tX")         == Left(InvalidNameProblem("RelativeExecutablePath", "\tX")))
    assert(ExecutablePath.checked("X\n")         == Left(InvalidNameProblem("RelativeExecutablePath", "X\n")))
    assert(ExecutablePath.checked(".")           == Left(InvalidNameProblem("RelativeExecutablePath", ".")))
    assert(ExecutablePath.checked("../file")     == Left(InvalidNameProblem("RelativeExecutablePath", "../file")))
    assert(ExecutablePath.checked("./file")      == Left(InvalidNameProblem("RelativeExecutablePath", "./file")))
    assert(ExecutablePath.checked("dir/./file")  == Left(InvalidNameProblem("RelativeExecutablePath", "dir/./file")))
    assert(ExecutablePath.checked(".hidden")     == Left(InvalidNameProblem("RelativeExecutablePath", ".hidden")))
    //assert(ExecutablePath.checked("""/a\b""")     == Left(InvalidNameProblem("ExecutablePath", """/a\b""")))
    assert(ExecutablePath.checked("dir/../file") == Left(InvalidNameProblem("RelativeExecutablePath", "dir/../file")))
    assert(ExecutablePath.checked("dir\\../file") == Left(InvalidNameProblem("RelativeExecutablePath", "dir\\../file")))
    assert(ExecutablePath.checked("dir/./file")  == Left(InvalidNameProblem("RelativeExecutablePath", "dir/./file")))
  }

  "Valid ExecutablePath" in {
    assert(ExecutablePath.checked("/file")     == Right(AbsoluteExecutablePath("/file")))
    assert(ExecutablePath.checked("file")      == Right(RelativeExecutablePath("file")))
    assert(ExecutablePath.checked("/dir/file") == Right(AbsoluteExecutablePath("/dir/file")))
    assert(ExecutablePath.checked("dir/file")  == Right(RelativeExecutablePath("dir/file")))
    assert(ExecutablePath.checked("/file/")    == Right(AbsoluteExecutablePath("/file/")))   // ?
    assert(ExecutablePath.checked("file/")     == Right(RelativeExecutablePath("file/")))   // ?
    assert(ExecutablePath.checked("/")         == Right(AbsoluteExecutablePath("/")))
    assert(ExecutablePath.checked("/.").isRight)
    assert(ExecutablePath.checked("/../file").isRight)
    assert(ExecutablePath.checked("/./file").isRight)
    assert(ExecutablePath.checked("/dir/./file").isRight)
    assert(ExecutablePath.checked("/").isRight)
    assert(ExecutablePath.checked("/.hidden").isRight)
    assert(ExecutablePath.checked("""/a\b""").isRight)
    assert(ExecutablePath.checked("/dir/../file").isRight)
    assert(ExecutablePath.checked("/dir/./file").isRight)
  }
}
