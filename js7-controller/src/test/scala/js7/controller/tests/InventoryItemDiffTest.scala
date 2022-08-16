package js7.controller.tests

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJsonObject}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.test.Test
import js7.controller.tests.InventoryItemDiffTest.*
import js7.core.item.VersionedItemReader
import js7.data.item.InventoryItemDiff.diffItems
import js7.data.item.ItemChange.{AddedOrChanged, Removed}
import js7.data.item.{InventoryItemDiff, SourceType, TestPath, TestVersionedItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.workflow.instructions.{ExplicitEnd, Fail}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
final class InventoryItemDiffTest extends Test
{
  "diffItems" - {
    "empty" in {
      assert(diffItems(Nil, Nil).isEmpty)
    }

    lazy val a = Workflow.of(WorkflowPath("A") ~ "1", Fail(None))
    lazy val b = Workflow.of(WorkflowPath("B") ~ "1", Fail(None))
    lazy val c = Workflow.of(WorkflowPath("C") ~ "1", Fail(None))

    "different order" in {
      assert(
        diffItems(
          a :: b :: Nil,
          b :: a :: Nil
        ).isEmpty)
    }

    "one added" in {
      assert(
        diffItems(
          a :: b :: c :: Nil,
          a :: b :: Nil)
        == AddedOrChanged(c) :: Nil)
    }

    "one deleted" in {
      assert(
        diffItems(
          a :: Nil,
          a :: b :: Nil)
        == Removed(b.path) :: Nil)
    }

    "one changed" in {
      val bUpdated = Workflow.of(WorkflowPath("B") ~ "1", Fail(None), Fail(None))
      assert(bUpdated != b)
      assert(
        diffItems(
          a :: bUpdated :: Nil,
          a :: b :: Nil
        ) == AddedOrChanged(bUpdated) :: Nil)
    }

    "added, deleted and changed" in {
      val aUpdated = Workflow.of(WorkflowPath("A") ~ "1", Fail(None), Fail(None))
      assert(aUpdated != a)
      assert(
        diffItems(
          aUpdated :: c :: Nil,
          a :: b :: Nil
        ).toSet == Set(
          AddedOrChanged(aUpdated),
          Removed(b.path),
          AddedOrChanged(c)))
    }

    "version updated" in {
      assert(
        diffItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil
        ) == AddedOrChanged(b withVersion VersionId("CHANGED")) :: Nil)
    }

    "version updated, ignoreVersion=true" in {
      assert(
        diffItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil,
          ignoreVersion = true
        ) == Nil)
    }
  }

  "Diff" in {
    val diff = InventoryItemDiff.fromItemChanges(
      List(
        Removed(BWorkflow.path),
        AddedOrChanged(BTestItem.withVersion(V0)),
        AddedOrChanged(CWorkflow withVersion V1),
        AddedOrChanged(D1Workflow withVersion V1)))

    assert(diff == InventoryItemDiff[VersionedItemPath, VersionedItem](
      addedOrChanged = List(
        BTestItem.withVersion(V0),
        CWorkflow.withVersion(V1),
        D1Workflow.withVersion(V1)),
      removed = List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      InventoryItemDiff[WorkflowPath, Workflow](
        addedOrChanged = List(CWorkflow.withVersion(V1), D1Workflow.withVersion(V1)),
        removed = List(BWorkflow.path)))

    assert(diff.select[TestPath, TestVersionedItem] ==
      InventoryItemDiff[TestPath, TestVersionedItem](
        addedOrChanged = List(BTestItem withVersion V0),
        removed = Nil))
  }
}

object InventoryItemDiffTest
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
    val companion: TestVersionedItem.type = TestVersionedItem

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
