package com.sos.jobscheduler.data.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.data.filebased.TypedPath.VersionSeparator
import com.sos.jobscheduler.data.filebased.TypedPathTest._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathTest extends FreeSpec {

  "validate" in {
    intercept[ProblemException] { APath("") }
    intercept[ProblemException] { APath("x") }
    intercept[ProblemException] { APath("/x/") }
    intercept[ProblemException] { APath("x/") }
    intercept[ProblemException] { APath("/x//y") }
  }

  "check" in {
    assert(APath.checked("x") == Invalid(Problem("Absolute path expected in APath 'x'")))
  }

  "name" in {
    assert(APath("/name").name == "name")
    assert(APath("/a/b/name").name == "name")
  }

  "nesting" in {
    assert(APath("/a").nesting == 1)
    assert(APath("/a/b").nesting == 2)
  }

  "withoutStartingSlash" in {
    assert(APath("/a").withoutStartingSlash == "a")
  }

  "withTrailingSlash" in {
    assert(APath("/a").withTrailingSlash == "/a/")
  }

  "asTyped" in {
    val jobPath = APath("/TEST")
    assert(jobPath.asTyped[APath] eq jobPath)
    assert(BPath("/TEST").asTyped[APath] == jobPath)
    intercept[ProblemException] {
      APath("/TEST,1").asTyped[BPath]
    }
  }

  "makeAbsolute" in {
    assert(APath.makeAbsolute("a") == APath("/a"))
    assert(APath.makeAbsolute("a/b") == APath("/a/b"))
    intercept[IllegalArgumentException] { APath.makeAbsolute("./b") }
  }

  "fromFile" in {
    assert(APath.fromFile("x").isEmpty)
    assert(APath.fromFile("x.a.json") == Some(Valid(APath("/x") → SourceType.Json)))
    assert(APath.fromFile("x.a.txt") == Some(Valid(APath("/x") → SourceType.Txt)))
    assert(APath.fromFile("x.b.json") == None)
    assert(BPath.fromFile("x.b.json") == Some(Valid(BPath("/x") → SourceType.Json)))
    assert(BPath.fromFile(".b.json") == Some(Invalid(Problem("Trailing slash not allowed in BPath '/'"))))
  }

  "checkedNameSyntax" in {
    assert(APath("/folder/a-b").checkedNameSyntax == Valid(APath("/folder/a-b")))
    assert(APath("/folder/a_b").checkedNameSyntax == Valid(APath("/folder/a_b")))
    assert(APath("/folder/a.b").checkedNameSyntax == Valid(APath("/folder/a.b")))
    assert(APath("/a@b/x@y").checkedNameSyntax ==  // Show only first problem
      Invalid(Problem("Problem with 'A:/a@b/x@y': Invalid character or character combination in name 'a@b'")))
    assert(APath(s"/folder/a${VersionSeparator}b").checkedNameSyntax ==
      Invalid(Problem("Problem with 'A:/folder/a@b': Invalid character or character combination in name 'a@b'")))
  }

  "typedPathCodec" in {
    implicit val typedPathCodec = TypedPath.jsonCodec(List(APath, BPath))
    testJson[TypedPath](APath("/a"), json""" "A:/a" """)
    testJson[TypedPath](BPath("/b"), json""" "B:/b" """)
  }
}

private object TypedPathTest {
  final case class APath(string: String) extends TypedPath {
    validate()
    def companion = APath
  }
  object APath extends TypedPath.Companion[APath] {
    val sourceTypeToFilenameExtension = Map(
      SourceType.Json → s".a.json",
      SourceType.Txt → s".a.txt")
  }

  final case class BPath(string: String) extends TypedPath {
    validate()
    def companion = BPath
  }
  object BPath extends TypedPath.Companion[BPath] {
    val sourceTypeToFilenameExtension: Map[SourceType, String] = Map(
      SourceType.Json → ".b.json")
  }
}
