package com.sos.jobscheduler.data.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.data.filebased.TypedPath.VersionSeparator
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathTest extends FreeSpec {

  "JSON" in {
    testJson(APath("/PATH"), json""" "/PATH" """)
    intercept[IllegalArgumentException] {
      APath.Anonymous.asJson
    }
    assert(json""" "/?/anonymous" """.as[APath].isLeft)
    assert(json""" "/?/okay" """.as[APath].isRight)
  }

  "JSON with generic TypedPath.jsonCodec" in {
    implicit val typedPathCodec = TypedPath.jsonCodec(List(APath, BPath))
    testJson[TypedPath](APath("/a"), json""" "A:/a" """)
    testJson[TypedPath](BPath("/b"), json""" "B:/b" """)
  }

  "%" in {
    assert(APath("/PATH") % "VERSION"            == FileBasedId(APath("/PATH"), VersionId("VERSION")))
    assert(APath("/PATH") % VersionId("VERSION") == FileBasedId(APath("/PATH"), VersionId("VERSION")))
  }

  "validate" in {
    intercept[ProblemException] { APath("") }
    intercept[ProblemException] { APath("x") }
    intercept[ProblemException] { APath("/x/") }
    intercept[ProblemException] { APath("x/") }
    intercept[ProblemException] { APath("/x//y") }
    intercept[ProblemException] { APath("/x,y") }
    intercept[ProblemException] { APath("/x%y") }
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
    val aPath = APath("/TEST")
    assert(aPath.asTyped[APath] eq aPath)
    assert(BPath("/TEST").asTyped[APath] == aPath)
    intercept[ProblemException] {
      APath("/TEST,1").asTyped[BPath]
    }
  }

  "cast" in {
    val aPath = APath("/TEST")
    assert(aPath.cast[APath] eq aPath)
    intercept[ClassCastException] {
      aPath.cast[BPath]
    }
  }

  "makeAbsolute" in {
    assert(APath.makeAbsolute("a") == APath("/a"))
    assert(APath.makeAbsolute("a/b") == APath("/a/b"))
    intercept[IllegalArgumentException] { APath.makeAbsolute("./b") }
  }

  "fromFile" in {
    assert(APath.fromFile("?/anonymous.a.json") == Some(Invalid(Problem("Anonymous APath?"))))
    assert(APath.fromFile("x").isEmpty)
    assert(APath.fromFile("x.a.json") == Some(Valid(APath("/x") → SourceType.Json)))
    assert(APath.fromFile("x.a.txt") == Some(Valid(APath("/x") → SourceType.Txt)))
    assert(APath.fromFile("x.b.json") == None)
    assert(BPath.fromFile("x.b.json") == Some(Valid(BPath("/x") → SourceType.Json)))
    assert(BPath.fromFile(".b.json") == Some(Invalid(Problem("Trailing slash not allowed in BPath '/'"))))
  }

  "checked" in {
    assert(APath.checked("/?/anonymous") == Invalid(Problem("Anonymous APath?")))
  }

  "officialSyntaxChecked" in {
    assert(APath("/folder/a-b").officialSyntaxChecked == Valid(APath("/folder/a-b")))
    assert(APath("/folder/a_b").officialSyntaxChecked == Valid(APath("/folder/a_b")))
    assert(APath("/folder/a.b").officialSyntaxChecked == Valid(APath("/folder/a.b")))
    assert(APath("/a@b/x@y").officialSyntaxChecked ==  // Show only first problem
      Invalid(Problem("Problem with 'A:/a@b/x@y': Invalid character or character combination in name 'a@b'")))
    assert(APath.checked(s"/folder/a${VersionSeparator}b") ==
      Invalid(Problem("Contains a forbidden character: APath '/folder/a%b'")))
    //Shadowed: assert(APath(s"/folder/a${VersionSeparator}b").officialSyntaxChecked ==
    //  Invalid(Problem("Problem with 'A:/folder/a%b': Invalid character or character combination in name 'a%b'")))
  }

  "Internal" in {
    assert(APath("/?/TEST").officialSyntaxChecked == Invalid(Problem("Internal path is not allowed here: A:/?/TEST")))
  }

  "Anonymous" in {
    assert(APath.Anonymous == APath("/?/anonymous"))
    assert(APath.Anonymous.officialSyntaxChecked == Invalid(Problem("Internal path is not allowed here: A:/?/anonymous")))
    assert(APath.NoId == APath("/?/anonymous") % "⊥")
  }

  "name etc." in {
    assert(APath.name == "APath")
    assert(APath.toString == "APath")
    assert(APath.camelName == "A")
  }

  //"Versioned" in {
  //  testJson(
  //    APath.Versioned(VersionId("VERSION"), APath("/PATH")),
  //    json"""{
  //      "path": "/PATH",
  //      "versionId": "VERSION"
  //    }"""
  //  )
  //}
}
