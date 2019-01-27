package com.sos.jobscheduler.master

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.{FileBasedReader, Repo, TypedPaths}
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.MasterRepoReaderTest._
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec

final class MasterRepoReaderTest extends FreeSpec {

  "MasterRepoReader" in {
    provideDirectory { directory ⇒
      val repoReader = new MasterRepoReader(directory)
      var repo = Repo.empty
      assert(repo.idToFileBased.isEmpty)

      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(BWorkflowPath)
      repo = repoReader.applyConfigurationDirectory(repo, Some(V1)).orThrow
      assert(repo.idToFileBased == Map(
        (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
        (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1))))

      assert(repoReader.applyConfigurationDirectory(repo, Some(V1)) == Invalid(Problem("Duplicate VersionId '1'")))

      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(CWorkflowPath)
      (directory / "UNKNOWN.tmp").contentString = "?"
      (directory / "NO-JSON.workflow.json").contentString = "INVALID JSON"
      (directory / "ERROR-1.workflow.json").contentString = """{ "something": "different" }"""
      (directory / "ERROR-2.workflow.json").contentString = """{ "instructions": 0 }"""
      assert(repoReader.applyConfigurationDirectory(repo, Some(V2)) ==
        Invalid(Problem.Multiple(Set(
          TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp")),
          FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got I (line 1, column 1)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)"))))))

      delete(directory / "NO-JSON.workflow.json")
      delete(directory / "UNKNOWN.tmp")
      delete(directory / "ERROR-1.workflow.json")
      delete(directory / "ERROR-2.workflow.json")
      repo = repoReader.applyConfigurationDirectory(repo, Some(V2)).orThrow

      delete(directory / "B.workflow.json")
      repo = repoReader.applyConfigurationDirectory(repo, Some(V3)).orThrow

      assert(repo.versions == V3 :: V2 :: V1 :: Nil)
      assert(repo.idToFileBased == Map(
        (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
        (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1)),
        (BWorkflowPath % V3) → None,
        (CWorkflowPath % V2) → Some(TestWorkflow.withId(CWorkflowPath % V2))))

      def writeWorkflowFile(workflowPath: WorkflowPath): Unit =
        directory.resolve(workflowPath.toFile(SourceType.Json)).contentString = TestWorkflowJson.toString
    }
  }
}

object MasterRepoReaderTest {
  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")

  private val TestWorkflowJson = json"""
    {
      "instructions": [
        { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/EXECUTABLE", "taskLimit": 1 }}
      ]
    }"""
  private val TestWorkflow = TestWorkflowJson.as[Workflow].orThrow

  private def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
