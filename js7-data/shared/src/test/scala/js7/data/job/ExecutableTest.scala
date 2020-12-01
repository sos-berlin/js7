package js7.data.job

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import js7.data.value.expression.Expression.{Add, NamedValue, NumericConstant, ObjectExpression, StringConstant}
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecutableTest extends AnyFreeSpec
{
  "JSON" - {
    "RelativeExecutablePath, minimum" in {
      testJson[Executable](RelativeExecutablePath("EXECUTABLE"), json"""
        {
          "TYPE": "ExecutablePath",
          "path": "EXECUTABLE"
        }
      """)
    }

    "RelativeExecutablePath" in {
      testJson[Executable](
        RelativeExecutablePath(
          "EXECUTABLE",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> Add(NumericConstant(1), NumericConstant(2)))),
          v1Compatible = true),
        json"""
         {
          "TYPE": "ExecutablePath",
          "path": "EXECUTABLE",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "1 + 2"
          },
          "v1Compatible": true
        }
      """)
    }

    "AbsoluteExecutablePath, minimum" in {
      testJson[Executable](AbsoluteExecutablePath("/EXECUTABLE"), json"""
        {
          "TYPE": "ExecutablePath",
          "path": "/EXECUTABLE"
        }
      """)
    }

    "AbsoluteExecutablePath" in {
      testJson[Executable](
        AbsoluteExecutablePath(
          "/EXECUTABLE",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> NumericConstant(7))),
          v1Compatible = true),
        json"""
         {
          "TYPE": "ExecutablePath",
          "path": "/EXECUTABLE",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "7"
          },
          "v1Compatible": true
        }
      """)
    }

    "CommandLineExecutable" in {
      testJson[Executable](
        CommandLineExecutable(
          CommandLineExpression(
          """PROGRAM ARG-1 'ARG 2' "ARG 3" $ARG4 "$ARG5"""",
            List(
              StringConstant("PROGRAM"),
              StringConstant("ARG-1"),
              StringConstant("ARG 2"),
              StringConstant("ARG 3"),
              NamedValue.last("ARG4"),
              NamedValue.last("ARG5"))),
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> Add(NumericConstant(1), NumericConstant(2))))),
        json"""
        {
          "TYPE": "CommandLineExecutable",
           "command": "PROGRAM ARG-1 'ARG 2' \"ARG 3\" $$ARG4 \"$$ARG5\"",
           "env": {
             "ENV-VAR": "$$VAR",
             "NUMBER": "1 + 2"
           }
        }""")
    }

    "ExecutableScript, minumum" in {
      testJson[Executable](
        ExecutableScript("SCRIPT"),json"""
        {
          "TYPE": "ExecutableScript",
          "script": "SCRIPT"
        }
      """)
    }

    "ExecutableScript" in {
      testJson[Executable](
        ExecutableScript(
          "SCRIPT",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> NumericConstant(7))),
          v1Compatible = true),
      json"""
        {
          "TYPE": "ExecutableScript",
          "script": "SCRIPT",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "7"
          },
          "v1Compatible": true
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
