package js7.master.tests

import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import js7.base.problem.Checked.Ops
import js7.base.web.Uri
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.FileBaseds.diffFileBaseds
import js7.data.filebased.{FileBased, FileBaseds, RepoChange, TypedPath, VersionId}
import js7.data.job.ExecutablePath
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.tests.FileBasedsTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedsTest extends AnyFreeSpec
{
  "diffFileBaseds" - {
    "empty"  in {
      assert(diffFileBaseds(Nil, Nil).isEmpty)
    }

    lazy val a = AgentRef(AgentRefPath("/A") ~ "1", Uri("http://a"))
    lazy val b = AgentRef(AgentRefPath("/B") ~ "1", Uri("http://b"))
    lazy val c = AgentRef(AgentRefPath("/C") ~ "1", Uri("http://c"))

    "different order" in {
      assert(
        diffFileBaseds(
          a :: b :: Nil,
          b :: a :: Nil
        ).isEmpty)
    }

    "one added" in {
      assert(
        diffFileBaseds(
          a :: b :: c :: Nil,
          a :: b :: Nil)
        == RepoChange.Added(c) :: Nil)
    }

    "one deleted" in {
      assert(
        diffFileBaseds(
          a :: Nil,
          a :: b :: Nil)
        == RepoChange.Deleted(b.path) :: Nil)
    }

    "one updated" in {
      assert(
        diffFileBaseds(
          a :: b.copy(uri = Uri("http://B-CHANGED")) :: Nil,
          a :: b :: Nil
        ) == RepoChange.Updated(b.copy(uri = Uri("http://B-CHANGED"))) :: Nil)
    }

    "added, deleted and updated" in {
      assert(
        diffFileBaseds(
          a.copy(uri = Uri("http://A-CHANGED")) :: c :: Nil,
          a :: b :: Nil
        ).toSet == Set(
          RepoChange.Updated(a.copy(uri = Uri("http://A-CHANGED"))),
          RepoChange.Deleted(b.path),
          RepoChange.Added(c)))
    }

    "version updated" in {
      assert(
        diffFileBaseds(
          a :: b. withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil
        ) == RepoChange.Updated(b withVersion VersionId("CHANGED")) :: Nil)
    }

    "version updated, ignoreVersion=true" in {
      assert(
        diffFileBaseds(
          a :: b. withVersion(VersionId("CHANGED")) :: Nil,
          a :: b :: Nil,
          ignoreVersion = true
        ) == Nil)
    }
  }

  "Diff" in {
    val diff = FileBaseds.Diff.fromRepoChanges(
      List(
        RepoChange.Deleted(BWorkflow.path),
        RepoChange.Added(BAgent withVersion V0),
        RepoChange.Added(CWorkflow withVersion V1),
        RepoChange.Updated(D1Workflow withVersion V1)))

    assert(diff == FileBaseds.Diff[TypedPath, FileBased](
      added = List(BAgent withVersion V0, CWorkflow withVersion V1),
      updated = List(D1Workflow withVersion V1),
      deleted = List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      FileBaseds.Diff[WorkflowPath, Workflow](
        added = List(CWorkflow withVersion V1),
        updated = List(D1Workflow withVersion V1),
        deleted = List(BWorkflow.path)))

    assert(diff.select[AgentRefPath, AgentRef] ==
      FileBaseds.Diff[AgentRefPath, AgentRef](
        added = List(BAgent withVersion V0),
        updated = Nil,
        deleted = Nil))
  }
}

object FileBasedsTest {
  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("/A"))
  private[tests] val BWorkflow = Workflow(WorkflowPath("/B"), Vector("B-END" @: ExplicitEnd()))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("/C"), "define workflow { /*EMPTY*/ }").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("/D"), Vector("D-END" @: ExplicitEnd()))
  private[tests] val EWorkflow = Workflow(WorkflowPath("/E"), Vector(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE")))))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("/D"), "define workflow { CHANGED-D-END: end; }").orThrow
  private[tests] val AAgent = AgentRef(AgentRefPath("/A"), Uri("http://A"))
  private[tests] val BAgent = AgentRef(AgentRefPath("/folder/B"), Uri("http://B"))

  private[tests] def provideDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
