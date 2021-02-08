package js7.data.job

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import js7.data.value.expression.Expression.{Add, NamedValue, NumericConstant, ObjectExpression, StringConstant}
import js7.data.value.{NumberValue, StringValue}
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecutableTest extends AnyFreeSpec
{
  "JSON" - {
    "RelativePathExecutable, minimum" in {
      testJson[Executable](RelativePathExecutable("EXECUTABLE"), json"""
        {
          "TYPE": "PathExecutable",
          "path": "EXECUTABLE"
        }
      """)
    }

    "RelativePathExecutable" in {
      testJson[Executable](
        RelativePathExecutable(
          "EXECUTABLE",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> Add(NumericConstant(1), NumericConstant(2)))),
          v1Compatible = true),
        json"""
         {
          "TYPE": "PathExecutable",
          "path": "EXECUTABLE",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "1 + 2"
          },
          "v1Compatible": true
        }
      """)
    }

    "AbsolutePathExecutable, minimum" in {
      testJson[Executable](AbsolutePathExecutable("/EXECUTABLE"), json"""
        {
          "TYPE": "PathExecutable",
          "path": "/EXECUTABLE"
        }
      """)
    }

    "AbsolutePathExecutable" in {
      testJson[Executable](
        AbsolutePathExecutable(
          "/EXECUTABLE",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> NumericConstant(7))),
          v1Compatible = true),
        json"""
         {
          "TYPE": "PathExecutable",
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

    "ScriptExecutable, minumum" in {
      testJson[Executable](
        ScriptExecutable("SCRIPT"),json"""
        {
          "TYPE": "ScriptExecutable",
          "script": "SCRIPT"
        }
      """)
    }

    "ScriptExecutable" in {
      testJson[Executable](
        ScriptExecutable(
          "SCRIPT",
          env = ObjectExpression(Map(
            "ENV-VAR" -> NamedValue.last("VAR"),
            "NUMBER" -> NumericConstant(7))),
          v1Compatible = true),
      json"""
        {
          "TYPE": "ScriptExecutable",
          "script": "SCRIPT",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "7"
          },
          "v1Compatible": true
        }
      """)
    }

    "InternalExecutable" in {
      testJson[Executable](
        InternalExecutable(
          "js7.tests.jobs.EmptyJob",
          jobArguments = Map(
            "ARG" -> StringValue("An Argument for the intantiated class"),
            "NUMBER" -> NumberValue(3)),
          arguments = ObjectExpression(Map(
            "MY-ARG" -> NamedValue.last("ARG"),
            "NUMBER" -> NumericConstant(7)))),
      json"""
        {
          "TYPE": "InternalExecutable",
          "className": "js7.tests.jobs.EmptyJob",
          "jobArguments": {
            "ARG": "An Argument for the intantiated class",
            "NUMBER": 3
          },
          "arguments": {
            "MY-ARG": "$$ARG",
            "NUMBER": "7"
          }
        }
      """)
    }
  }

  "Names before 2020-01-20" - {
    "ExecutableScript" in {
      testJsonDecoder[Executable](
        ScriptExecutable("SCRIPT"),
        json"""
          {
            "TYPE": "ExecutableScript",
            "script": "SCRIPT"
          }
        """)
    }

    "ExecutablePath"  in {
      testJsonDecoder[Executable](
        PathExecutable("PATH"),
        json"""
          {
            "TYPE": "ExecutablePath",
            "path": "PATH"
          }
        """)
    }
  }

  "Invalid PathExecutable" in {
    assert(PathExecutable.checked("")            == Left(EmptyStringProblem("RelativePathExecutable")))
    assert(PathExecutable.checked(" ")           == Left(InvalidNameProblem("RelativePathExecutable", " ")))
    assert(PathExecutable.checked(" X")          == Left(InvalidNameProblem("RelativePathExecutable", " X")))
    assert(PathExecutable.checked("\tX")         == Left(InvalidNameProblem("RelativePathExecutable", "\tX")))
    assert(PathExecutable.checked("X\n")         == Left(InvalidNameProblem("RelativePathExecutable", "X\n")))
    assert(PathExecutable.checked(".")           == Left(InvalidNameProblem("RelativePathExecutable", ".")))
    assert(PathExecutable.checked("../file")     == Left(InvalidNameProblem("RelativePathExecutable", "../file")))
    assert(PathExecutable.checked("./file")      == Left(InvalidNameProblem("RelativePathExecutable", "./file")))
    assert(PathExecutable.checked("dir/./file")  == Left(InvalidNameProblem("RelativePathExecutable", "dir/./file")))
    assert(PathExecutable.checked(".hidden")     == Left(InvalidNameProblem("RelativePathExecutable", ".hidden")))
    //assert(PathExecutable.checked("""/a\b""")     == Left(InvalidNameProblem("PathExecutable", """/a\b""")))
    assert(PathExecutable.checked("dir/../file") == Left(InvalidNameProblem("RelativePathExecutable", "dir/../file")))
    assert(PathExecutable.checked("dir\\../file") == Left(InvalidNameProblem("RelativePathExecutable", "dir\\../file")))
    assert(PathExecutable.checked("dir/./file")  == Left(InvalidNameProblem("RelativePathExecutable", "dir/./file")))
  }

  "Valid PathExecutable" in {
    assert(PathExecutable.checked("/file")     == Right(AbsolutePathExecutable("/file")))
    assert(PathExecutable.checked("file")      == Right(RelativePathExecutable("file")))
    assert(PathExecutable.checked("/dir/file") == Right(AbsolutePathExecutable("/dir/file")))
    assert(PathExecutable.checked("dir/file")  == Right(RelativePathExecutable("dir/file")))
    assert(PathExecutable.checked("/file/")    == Right(AbsolutePathExecutable("/file/")))   // ?
    assert(PathExecutable.checked("file/")     == Right(RelativePathExecutable("file/")))   // ?
    assert(PathExecutable.checked("/")         == Right(AbsolutePathExecutable("/")))
    assert(PathExecutable.checked("/.").isRight)
    assert(PathExecutable.checked("/../file").isRight)
    assert(PathExecutable.checked("/./file").isRight)
    assert(PathExecutable.checked("/dir/./file").isRight)
    assert(PathExecutable.checked("/").isRight)
    assert(PathExecutable.checked("/.hidden").isRight)
    assert(PathExecutable.checked("""/a\b""").isRight)
    assert(PathExecutable.checked("/dir/../file").isRight)
    assert(PathExecutable.checked("/dir/./file").isRight)
  }
}
