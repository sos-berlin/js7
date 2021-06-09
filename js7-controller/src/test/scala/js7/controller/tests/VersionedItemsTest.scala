package js7.controller.tests

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJsonObject}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked.Ops
import js7.controller.tests.VersionedItemsTest._
import js7.core.item.VersionedItemReader
import js7.data.item.VersionedItems.diffVersionedItems
import js7.data.item.{RepoChange, SourceType, TestPath, TestVersionedItem, VersionId, VersionedItem, VersionedItemPath, VersionedItems}
import js7.data.workflow.instructions.{ExplicitEnd, Fail}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemsTest extends AnyFreeSpec
{
  "diffVersionedItems" - {
    "empty"  in {
      assert(diffVersionedItems(Nil, Nil).isEmpty)
    }

    lazy val a = Workflow.of(WorkflowPath("A") ~ "1", Fail(None))
    lazy val b = Workflow.of(WorkflowPath("B") ~ "1", Fail(None))
    lazy val c = Workflow.of(WorkflowPath("C") ~ "1", Fail(None))

    "different order" in {
      assert(
        diffVersionedItems(
          a :: b :: Nil,
          b :: a :: Nil
        ).isEmpty)
    }

    "one added" in {
      assert(
        diffVersionedItems(
          a :: b :: c :: Nil,
          a :: b :: Nil)
        == RepoChange.Added(c) :: Nil)
    }

    "one deleted" in {
      assert(
        diffVersionedItems(
          a :: Nil,
          a :: b :: Nil)
        == RepoChange.Removed(b.path) :: Nil)
    }

    "one changed" in {
      val bUpdated = Workflow.of(WorkflowPath("B") ~ "1", Fail(None), Fail(None))
      assert(bUpdated != b)
      assert(
        diffVersionedItems(
          a :: bUpdated :: Nil,
          a :: b :: Nil
        ) == RepoChange.Changed(bUpdated) :: Nil)
    }

    "added, deleted and changed" in {
      val aUpdated = Workflow.of(WorkflowPath("A") ~ "1", Fail(None), Fail(None))
      assert(aUpdated != a)
      assert(
        diffVersionedItems(
          aUpdated :: c :: Nil,
          a :: b :: Nil
        ).toSet == Set(
          RepoChange.Changed(aUpdated),
          RepoChange.Removed(b.path),
          RepoChange.Added(c)))
    }

    "version updated" in {
      assert(
        diffVersionedItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil
        ) == RepoChange.Changed(b withVersion VersionId("CHANGED")) :: Nil)
    }

    "version updated, ignoreVersion=true" in {
      assert(
        diffVersionedItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil,
          ignoreVersion = true
        ) == Nil)
    }
  }

  "Diff" in {
    val diff = VersionedItems.Diff.fromRepoChanges(
      List(
        RepoChange.Removed(BWorkflow.path),
        RepoChange.Added(BTestItem withVersion V0),
        RepoChange.Added(CWorkflow withVersion V1),
        RepoChange.Changed(D1Workflow withVersion V1)))

    assert(diff == VersionedItems.Diff[VersionedItemPath, VersionedItem](
      added = List(BTestItem withVersion V0, CWorkflow withVersion V1),
      changed = List(D1Workflow withVersion V1),
      removed = List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      VersionedItems.Diff[WorkflowPath, Workflow](
        added = List(CWorkflow withVersion V1),
        changed = List(D1Workflow withVersion V1),
        removed = List(BWorkflow.path)))

    assert(diff.select[TestPath, TestVersionedItem] ==
      VersionedItems.Diff[TestPath, TestVersionedItem](
        added = List(BTestItem withVersion V0),
        changed = Nil,
        removed = Nil))
  }
}

object VersionedItemsTest
{
  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("A"))
  private[tests] val BWorkflow = Workflow(WorkflowPath("B"), Vector("B-END" @: ExplicitEnd()))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("C"), "define workflow { /*EMPTY*/ }").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("D"), Vector("D-END" @: ExplicitEnd()))
  private[tests] val EWorkflow = Workflow(WorkflowPath("E"), Vector(Fail(None)))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("D"), "define workflow { `CHANGED-D-END`: end; }").orThrow
  private[tests] val ATestItem = TestVersionedItem(TestPath("A"), "A")
  private[tests] val BTestItem = TestVersionedItem(TestPath("folder/B"), "B")

  private[tests] def provideDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }

  private[tests] object TestItemReader extends VersionedItemReader
  {
    val companion = TestVersionedItem

    protected def read(testId: TestVersionedItem.Key, byteArray: ByteArray) = {
      case t: SourceType.JsonLike =>
        readAnonymousJsonLike(t, byteArray).map(_ withId testId)
    }

    def convertFromJson(json: Json) =
      Json.fromJsonObject(
        json.asObject.get ++ JsonObject("id" -> (TestPath.Anonymous ~ VersionId.Anonymous).asJson)
      ).as[TestVersionedItem].toChecked
  }
}
