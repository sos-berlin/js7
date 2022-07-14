package js7.data.item

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.problem.{Problem, ProblemException}
import js7.data.item.VersionedItemId.VersionSeparator
import js7.data.item.VersionedItemPathTest.*
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemPathTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(APath("PATH"), json""" "PATH" """)
    intercept[IllegalArgumentException] {
      APath.Anonymous.asJson
    }
    assert(json""" "⊥" """.as[APath].isLeft)
    assert(json""" "${APath.Anonymous.string}" """.as[APath].isLeft)
    assert(json""" "//ERROR" """.as[APath].isLeft)
    assert(json""" "PATH" """.as[APath] == Right(APath("PATH")))
    assert(json""" "🔵" """.as[APath] == Right(APath("🔵")))
  }

  "JSON with generic VersionedItemPath.jsonCodec" in {
    implicit val itemPathCodec = InventoryItemPath.jsonCodec[VersionedItemPath](List(APath, BPath))
    testJson[VersionedItemPath](APath("a"), json""" "A:a" """)
    testJson[VersionedItemPath](BPath("b"), json""" "B:b" """)
  }

  "~ operator" in {
    assert(APath("PATH") ~ "VERSION"            == VersionedItemId(APath("PATH"), VersionId("VERSION")))
    assert(APath("PATH") ~ VersionId("VERSION") == VersionedItemId(APath("PATH"), VersionId("VERSION")))
  }

  "apply throws" in {
    intercept[ProblemException] { APath("") }
    intercept[ProblemException] { APath("x/") }
    intercept[ProblemException] { APath("x/") }
    intercept[ProblemException] { APath("x//y") }
    intercept[ProblemException] { APath("x,y") }
    intercept[ProblemException] { APath("x~y") }
  }

  "name" in {
    assert(APath("name").name == "name")
    assert(APath("name").name == "name")
    assert(APath("a/name").name == "name")
    assert(APath("a/b/name").name == "name")
  }

  "string" in {
    assert(APath("a").string == "a")
  }

  "withTrailingSlash" in {
    assert(APath("a").withTrailingSlash == "a/")
  }

  "asTyped" in {
    val aPath = APath("TEST")
    assert(aPath.asTyped[APath] eq aPath)
    assert(BPath("TEST").asTyped[APath] == aPath)
    intercept[ProblemException] {
      APath("TEST,1").asTyped[BPath]
    }
  }

  "fromFile" in {
    assert(APath.fromFile("?/xx.a.json") == Some(Left(InvalidNameProblem("APath", "?/xx"))))
    assert(APath.fromFile("x").isEmpty)
    assert(APath.fromFile("x.a.json") == Some(Right(APath("x") -> SourceType.Json)))
    assert(APath.fromFile("x.a.txt") == Some(Right(APath("x") -> SourceType.Txt)))
    assert(APath.fromFile("x.b.json") == None)
    assert(BPath.fromFile("x.b.json") == Some(Right(BPath("x") -> SourceType.Json)))
    assert(BPath.fromFile(".b.json") == Some(Left(EmptyStringProblem("BPath"))))
  }

  "checked" in {
    assert(APath.checked("?") == Left(InvalidNameProblem("APath", "?")))
    assert(APath.checked("X") == Right(APath("X")))
    assert(APath.checked("?/xx") == Left(InvalidNameProblem("APath", "?/xx")))
    assert(APath.checked("/folder/a-b").isLeft)
    assert(APath.checked("folder/a-b") == Right(APath("folder/a-b")))
    assert(APath.checked("a@b") == Left(InvalidNameProblem("APath", "a@b")))
    assert(APath.checked("a,b") == Left(InvalidNameProblem("APath", "a,b")))
    assert(APath.checked("a//b") == Left(InvalidNameProblem("APath", "a//b")))
    assert(APath.checked("🔵") == Right(APath("🔵")))
    assert(APath.checked(s"a${VersionSeparator}b") == Left(InvalidNameProblem("APath", "a~b")))
  }

  "AId.checked" in {
    assert(AId.checked("A~1") == Right(APath("A") ~ VersionId("1")))
    // TODO Restrict VersionId syntax?
    assert(AId.checked("A~1~2") == Right(APath("A") ~ VersionId("1~2")))
    assert(AId.checked("A/B~1~2") == Right(APath("A/B") ~ VersionId("1~2")))
    assert(AId.checked("A~1/B~2") == Right(APath("A") ~ VersionId("1/B~2")))
    assert(AId.checked("A") == Left(Problem("APath without version (denoted by '~')?: A")))
  }

  "Anonymous" in {
    assert(APath.Anonymous == APath.unchecked("⊥"))
    assert(APath.NoId == APath.unchecked("⊥") ~ VersionId.unchecked("⊥"))
    assert(APath.checked(APath.Anonymous.string) == Left(Problem("Anonymous APath not allowed here")))
  }

  "name etc." in {
    assert(APath.name == "APath")
    assert(APath.toString == "APath")
    assert(APath.itemTypeName == "A")
  }

  "pattern matching with `VersionedItem.as`" in {
    def u(id: VersionedItemId_) = id match {
      case AId.as(id) =>
        assert(id.path.isInstanceOf[APath])
        "A"
      case BId.as(id) =>
        assert(id.path.isInstanceOf[BPath])
        "B"
      case _ => fail()
    }
    assert(u(APath("a") ~ "1") == "A")
    assert(u(BPath("b") ~ "1") == "B")
  }
}

object VersionedItemPathTest
{
  private type AId = VersionedItemId[APath]
  private val AId = APath.VersionedItemIdCompanion

  private type BId = VersionedItemId[BPath]
  private val BId = BPath.VersionedItemIdCompanion
}
