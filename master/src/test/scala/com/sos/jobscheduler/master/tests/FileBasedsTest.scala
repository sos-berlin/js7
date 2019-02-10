package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.FileBaseds
import com.sos.jobscheduler.core.filebased.FileBaseds.diffFileBaseds
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoChange, TypedPath, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ExplicitEnd}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.tests.FileBasedsTest._
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedsTest extends FreeSpec
{
  "diffFileBaseds" - {
    "empty"  in {
      assert(diffFileBaseds(Nil, Nil).isEmpty)
    }

    lazy val a = Agent(AgentPath("/A") % "1", "http://a")
    lazy val b = Agent(AgentPath("/B") % "1", "http://b")
    lazy val c = Agent(AgentPath("/C") % "1", "http://c")

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
          a :: b.copy(uri = "http://B-CHANGED") :: Nil,
          a :: b :: Nil
        ) == RepoChange.Updated(b.copy(uri = "http://B-CHANGED")) :: Nil)
    }

    "added, deleted and updated" in {
      assert(
        diffFileBaseds(
          a.copy(uri = "http://A-CHANGED") :: c :: Nil,
          a :: b :: Nil
        ).toSet == Set(
          RepoChange.Updated(a.copy(uri = "http://A-CHANGED")),
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

    assert(diff.select[AgentPath, Agent] ==
      FileBaseds.Diff[AgentPath, Agent](
        added = List(BAgent withVersion V0),
        updated = Nil,
        deleted = Nil))
  }
}

object FileBasedsTest {
  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("/A"))
  private[tests] val BWorkflow = Workflow(WorkflowPath("/B"), Vector("B-END" @: ExplicitEnd))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("/C"), "define workflow { /*EMPTY*/ }").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("/D"), Vector("D-END" @: ExplicitEnd))
  private[tests] val EWorkflow = Workflow(WorkflowPath("/E"), Vector(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE")))))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("/D"), "define workflow { CHANGED-D-END: end; }").orThrow
  private[tests] val AAgent = Agent(AgentPath("/A"), "http://A")
  private[tests] val BAgent = Agent(AgentPath("/folder/B"), "http://B")

  private[tests] def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
