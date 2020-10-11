package js7.controller.tests

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.{RichCirceEither, RichJsonObject, deriveCodec}
import js7.base.data.ByteArray
import js7.base.problem.Checked.Ops
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.tests.IntentoryItemsTest._
import js7.core.item.InventoryItemReader
import js7.data.item.IntentoryItems.diffInventoryItems
import js7.data.item.{IntentoryItems, InventoryItem, ItemId, RepoChange, SourceType, TypedPath, VersionId}
import js7.data.workflow.instructions.{ExplicitEnd, Fail}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IntentoryItemsTest extends AnyFreeSpec
{
  "diffInventoryItems" - {
    "empty"  in {
      assert(diffInventoryItems(Nil, Nil).isEmpty)
    }

    lazy val a = Workflow.of(WorkflowPath("/A") ~ "1", Fail(None))
    lazy val b = Workflow.of(WorkflowPath("/B") ~ "1", Fail(None))
    lazy val c = Workflow.of(WorkflowPath("/C") ~ "1", Fail(None))

    "different order" in {
      assert(
        diffInventoryItems(
          a :: b :: Nil,
          b :: a :: Nil
        ).isEmpty)
    }

    "one added" in {
      assert(
        diffInventoryItems(
          a :: b :: c :: Nil,
          a :: b :: Nil)
        == RepoChange.Added(c) :: Nil)
    }

    "one deleted" in {
      assert(
        diffInventoryItems(
          a :: Nil,
          a :: b :: Nil)
        == RepoChange.Deleted(b.path) :: Nil)
    }

    "one updated" in {
      val bUpdated = Workflow.of(WorkflowPath("/B") ~ "1", Fail(None), Fail(None))
      assert(bUpdated != b)
      assert(
        diffInventoryItems(
          a :: bUpdated :: Nil,
          a :: b :: Nil
        ) == RepoChange.Updated(bUpdated) :: Nil)
    }

    "added, deleted and updated" in {
      val aUpdated = Workflow.of(WorkflowPath("/A") ~ "1", Fail(None), Fail(None))
      assert(aUpdated != a)
      assert(
        diffInventoryItems(
          aUpdated :: c :: Nil,
          a :: b :: Nil
        ).toSet == Set(
          RepoChange.Updated(aUpdated),
          RepoChange.Deleted(b.path),
          RepoChange.Added(c)))
    }

    "version updated" in {
      assert(
        diffInventoryItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil
        ) == RepoChange.Updated(b withVersion VersionId("CHANGED")) :: Nil)
    }

    "version updated, ignoreVersion=true" in {
      assert(
        diffInventoryItems(
          a :: b.withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil,
          ignoreVersion = true
        ) == Nil)
    }
  }

  "Diff" in {
    val diff = IntentoryItems.Diff.fromRepoChanges(
      List(
        RepoChange.Deleted(BWorkflow.path),
        RepoChange.Added(BTestItem withVersion V0),
        RepoChange.Added(CWorkflow withVersion V1),
        RepoChange.Updated(D1Workflow withVersion V1)))

    assert(diff == IntentoryItems.Diff[TypedPath, InventoryItem](
      added = List(BTestItem withVersion V0, CWorkflow withVersion V1),
      updated = List(D1Workflow withVersion V1),
      deleted = List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      IntentoryItems.Diff[WorkflowPath, Workflow](
        added = List(CWorkflow withVersion V1),
        updated = List(D1Workflow withVersion V1),
        deleted = List(BWorkflow.path)))

    assert(diff.select[TestPath, TestItem] ==
      IntentoryItems.Diff[TestPath, TestItem](
        added = List(BTestItem withVersion V0),
        updated = Nil,
        deleted = Nil))
  }
}

object IntentoryItemsTest {
  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("/A"))
  private[tests] val BWorkflow = Workflow(WorkflowPath("/B"), Vector("B-END" @: ExplicitEnd()))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("/C"), "define workflow { /*EMPTY*/ }").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("/D"), Vector("D-END" @: ExplicitEnd()))
  private[tests] val EWorkflow = Workflow(WorkflowPath("/E"), Vector(Fail(None)))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("/D"), "define workflow { CHANGED-D-END: end; }").orThrow
  private[tests] val ATestItem = TestItem(TestPath("/A"), "A")
  private[tests] val BTestItem = TestItem(TestPath("/folder/B"), "B")

  private[tests] def provideDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }

  private[tests] case class TestPath(string: String) extends TypedPath {
    def companion = TestPath
  }
  private[tests] object TestPath extends TypedPath.Companion[TestPath] {
    val sourceTypeToFilenameExtension = Map(
      SourceType.Json -> ".test.json")

    protected def unchecked(string: String) = new TestPath(string)
  }

  private[tests] type TestId = ItemId[TestPath]
  private[tests] val TestId = new ItemId.Companion[TestPath] {}

  private[tests] final case class TestItem(id: TestId, content: String) extends InventoryItem {
    type Self = TestItem
    val companion = TestItem

    def withId(id: ItemId[Path]) = copy(id)
  }
  private[tests] object TestItem extends InventoryItem.Companion[TestItem] {
    type ThisItem = TestItem
    type Path = TestPath
    def typedPathCompanion = TestPath
    implicit val jsonCodec = deriveCodec[TestItem]
  }

  private[tests] object TestItemReader extends InventoryItemReader
  {
    val companion = TestItem

    protected def read(testId: ItemId[TestPath], byteArray: ByteArray) = {
      case t: SourceType.JsonLike =>
        readAnonymousJsonLike(t, byteArray).map(_ withId testId)
    }

    def convertFromJson(json: Json) =
      Json.fromJsonObject(
        json.asObject.get ++ JsonObject("id" -> (TestPath.Anonymous ~ VersionId.Anonymous).asJson)
      ).as[TestItem].toChecked
  }
}
