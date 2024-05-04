package js7.data.job

import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.io.process.KeyLogin
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.{Add, NamedValue, NumericConstant, StringConstant}
import js7.tester.CirceJsonTester.*

/**
  * @author Joacim Zschimmer
  */
final class ExecutableTest extends OurTestSuite:

  "JSON" - {
    "RelativePathExecutable, minimum" in:
      testJson[Executable](RelativePathExecutable("EXECUTABLE"), json"""
        {
          "TYPE": "PathExecutable",
          "path": "EXECUTABLE"
        }
      """)

    "RelativePathExecutable" in:
      testJson[Executable](
        RelativePathExecutable(
          "EXECUTABLE",
          env = Map(
            "ENV-VAR" -> NamedValue("VAR"),
            "NUMBER" -> Add(NumericConstant(1), NumericConstant(2))),
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

    "AbsolutePathExecutable, minimum" in:
      testJson[Executable](AbsolutePathExecutable("/EXECUTABLE"), json"""
        {
          "TYPE": "PathExecutable",
          "path": "/EXECUTABLE"
        }
      """)

    "AbsolutePathExecutable" in:
      testJson[Executable](
        AbsolutePathExecutable(
          "/EXECUTABLE",
          env = Map(
            "ENV-VAR" -> NamedValue("VAR"),
            "NUMBER" -> NumericConstant(7)),
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

    "CommandLineExecutable" in:
      testJson[Executable](
        CommandLineExecutable(
          CommandLineExpression(
          """PROGRAM ARG-1 'ARG 2' "ARG 3" $ARG4 "$ARG5"""",
            List(
              StringConstant("PROGRAM"),
              StringConstant("ARG-1"),
              StringConstant("ARG 2"),
              StringConstant("ARG 3"),
              NamedValue("ARG4"),
              NamedValue("ARG5"))),
          env = Map(
            "ENV-VAR" -> NamedValue("VAR"),
            "NUMBER" -> Add(NumericConstant(1), NumericConstant(2)))),
        json"""
        {
          "TYPE": "CommandLineExecutable",
           "command": "PROGRAM ARG-1 'ARG 2' \"ARG 3\" $$ARG4 \"$$ARG5\"",
           "env": {
             "ENV-VAR": "$$VAR",
             "NUMBER": "1 + 2"
           }
        }""")

    "ShellScriptExecutable, minumum" in:
      testJson[Executable](
        ShellScriptExecutable("SCRIPT"),json"""
        {
          "TYPE": "ShellScriptExecutable",
          "script": "SCRIPT"
        }
      """)

    "ShellScriptExecutable" in:
      testJson[Executable](
        ShellScriptExecutable(
          "SCRIPT",
          env = Map(
            "ENV-VAR" -> NamedValue("VAR"),
            "NUMBER" -> NumericConstant(7)),
          login = Some(KeyLogin("CREDENTIAL-KEY", withUserProfile = true)),
          v1Compatible = true),
      json"""
        {
          "TYPE": "ShellScriptExecutable",
          "script": "SCRIPT",
          "env": {
            "ENV-VAR": "$$VAR",
            "NUMBER": "7"
          },
          "login": {
            "credentialKey": "CREDENTIAL-KEY",
            "withUserProfile": true
          },
          "v1Compatible": true
        }
      """)

    "InternalExecutable" in:
      testJsonDecoder[Executable](
        InternalExecutable("js7.tests.jobs.EmptyJob"),
      json"""
        {
          "TYPE": "InternalExecutable",
          "className": "js7.tests.jobs.EmptyJob"
        }
      """)
      testJson[Executable](
        InternalExecutable(
          "js7.tests.jobs.EmptyJob",
          script = "SCRIPT",
          jobArguments = Map(
            "ARG" -> StringConstant("An Argument for the instantiated class"),
            "NUMBER" -> NumericConstant(3)),
          arguments = Map(
            "MY-ARG" -> NamedValue("ARG"),
            "NUMBER" -> NumericConstant(7))),
      json"""
        {
          "TYPE": "InternalExecutable",
          "className": "js7.tests.jobs.EmptyJob",
          "script": "SCRIPT",
          "jobArguments": {
            "ARG": "'An Argument for the instantiated class'",
            "NUMBER": "3"
          },
          "arguments": {
            "MY-ARG": "$$ARG",
            "NUMBER": "7"
          }
        }
      """)
  }

  "Names before 2020-01-20" - {
    "ExecutableScript" in:
      testJsonDecoder[Executable](
        ShellScriptExecutable("SCRIPT"),
        json"""
          {
            "TYPE": "ExecutableScript",
            "script": "SCRIPT"
          }
        """)

    "ExecutablePath"  in:
      testJsonDecoder[Executable](
        PathExecutable("PATH"),
        json"""
          {
            "TYPE": "ExecutablePath",
            "path": "PATH"
          }
        """)
  }

  "Names before 2020-06-10" - {
    "ExecutableScript" in:
      testJsonDecoder[Executable](
        ShellScriptExecutable("SCRIPT"),
        json"""
          {
            "TYPE": "ScriptExecutable",
            "script": "SCRIPT"
          }
        """)
  }

  "Invalid PathExecutable" in:
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

  "Valid PathExecutable" in:
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
