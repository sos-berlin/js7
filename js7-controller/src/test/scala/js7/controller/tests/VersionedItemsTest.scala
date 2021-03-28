package js7.controller.tests

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Json, JsonObject}
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
import js7.data.item.{ItemPath, RepoChange, SourceType, VersionId, VersionedItem, VersionedItemId, VersionedItems}
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
        == RepoChange.Deleted(b.path) :: Nil)
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
          RepoChange.Deleted(b.path),
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
        RepoChange.Deleted(BWorkflow.path),
        RepoChange.Added(BTestItem withVersion V0),
        RepoChange.Added(CWorkflow withVersion V1),
        RepoChange.Changed(D1Workflow withVersion V1)))

    assert(diff == VersionedItems.Diff[ItemPath, VersionedItem](
      added = List(BTestItem withVersion V0, CWorkflow withVersion V1),
      changed = List(D1Workflow withVersion V1),
      deleted = List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      VersionedItems.Diff[WorkflowPath, Workflow](
        added = List(CWorkflow withVersion V1),
        changed = List(D1Workflow withVersion V1),
        deleted = List(BWorkflow.path)))

    assert(diff.select[TestPath, TestItem] ==
      VersionedItems.Diff[TestPath, TestItem](
        added = List(BTestItem withVersion V0),
        changed = Nil,
        deleted = Nil))
  }
}

object VersionedItemsTest {
  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("A"))
  private[tests] val BWorkflow = Workflow(WorkflowPath("B"), Vector("B-END" @: ExplicitEnd()))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("C"), "define workflow { /*EMPTY*/ }").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("D"), Vector("D-END" @: ExplicitEnd()))
  private[tests] val EWorkflow = Workflow(WorkflowPath("E"), Vector(Fail(None)))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("D"), "define workflow { `CHANGED-D-END`: end; }").orThrow
  private[tests] val ATestItem = TestItem(TestPath("A"), "A")
  private[tests] val BTestItem = TestItem(TestPath("folder/B"), "B")

  private[tests] def provideDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }

  private[tests] case class TestPath(string: String) extends ItemPath {
    def companion = TestPath
  }
  private[tests] object TestPath extends ItemPath.Companion[TestPath] {
    val sourceTypeToFilenameExtension = Map(
      SourceType.Json -> ".test.json")

    protected def unchecked(string: String) = new TestPath(string)
  }

  private[tests] type TestId = VersionedItemId[TestPath]
  //private[tests] val TestId = new VersionedItemId.Companion[TestPath] {}

  private[tests] final case class TestItem(id: TestId, content: String) extends VersionedItem {
    type Self = TestItem
    val companion = TestItem

    def withId(id: VersionedItemId[Path]) = copy(id)
  }
  private[tests] object TestItem extends VersionedItem.Companion[TestItem] {
    type Item = TestItem
    type Path = TestPath
    override type Id = TestId
    val cls = classOf[TestItem]
    val itemPathCompanion = TestPath
    implicit val jsonCodec: Codec.AsObject[TestItem] = deriveCodec[TestItem]
  }

  private[tests] object TestItemReader extends VersionedItemReader
  {
    val companion = TestItem

    protected def read(testId: VersionedItemId[TestPath], byteArray: ByteArray) = {
      case t: SourceType.JsonLike =>
        readAnonymousJsonLike(t, byteArray).map(_ withId testId)
    }

    def convertFromJson(json: Json) =
      Json.fromJsonObject(
        json.asObject.get ++ JsonObject("id" -> (TestPath.Anonymous ~ VersionId.Anonymous).asJson)
      ).as[TestItem].toChecked
  }
}
