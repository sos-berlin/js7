package com.sos.jobscheduler.tests.master.commands

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.core.filebased.{FileBasedReader, Repo, TypedPaths}
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.{ReadConfigurationDirectory, Response}
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.master.commands.ReadConfigurationDirectoryTest._
import java.nio.file.Files.{createDirectory, delete}
import java.nio.file.Paths
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ReadConfigurationDirectoryTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  protected val agentPaths = Nil
  protected val fileBased = Nil

  private lazy val live = directoryProvider.master.config / "live"

  override def beforeAll() = {
    directoryProvider.master.config / "private" / "private.conf" ++=
     s"""jobscheduler.auth.users {
        |  ${userAndPassword.userId.string} {
        |    password = "plain:${userAndPassword.password.string}"
        |  }
        |}
      """.stripMargin
    createDirectory(live)
    super.beforeAll()
    master.httpApi.login(userAndPassword.some).await(99.seconds)
  }

  "Initially, JobScheduler's Repo is empty" in {
    assert(repo.idToFileBased.isEmpty)
  }

  "Write two workflows" in {
    writeFile(AWorkflowPath)
    writeFile(BWorkflowPath)
    assert(readConfiguration(V1.some) == Valid(MasterCommand.Response.Accepted))
    assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToFileBased == Map(
      (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
      (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1))))
  }

  "Duplicate VersionId" in {
    assert(readConfiguration(V1.some) == Invalid(Problem("Duplicate VersionId '1'")))
  }

  "An unknown and some invalid files" in {
    writeFile(AWorkflowPath)
    writeFile(CWorkflowPath)
    (live / "UNKNOWN.tmp") := "?"
    (live / "NO-JSON.workflow.json") := "INVALID JSON"
    (live / "ERROR-1.workflow.json") := """{ "something": "different" }"""
    (live / "ERROR-2.workflow.json") := """{ "instructions": 0 }"""
    assert(readConfiguration(V2.some) ==
      Invalid(Problem.Multiple(Vector(
        FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
        FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)")),
        FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got I (line 1, column 1)")),
        TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp"))))))
  }

  "Some invalid files" in {
    delete(live / "UNKNOWN.tmp")
    assert(readConfiguration(V2.some) ==
      Invalid(Problem.Multiple(Vector(
        FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
        FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)")),
        FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got I (line 1, column 1)"))))))
  }

  "Delete invalid files" in {
    delete(live / "NO-JSON.workflow.json")
    delete(live / "ERROR-1.workflow.json")
    delete(live / "ERROR-2.workflow.json")
    readConfiguration(V2.some).orThrow
    assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToFileBased == Map(
      (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
      (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1)),
      (CWorkflowPath % V2) → Some(TestWorkflow.withId(CWorkflowPath % V2))))
  }

  "Delete a Workflow" in {
    delete(live / "B.workflow.json")
    readConfiguration(V3.some).orThrow
    assert(repo.versions == V3 :: V2 :: V1 :: VersionId("INITIAL") :: Nil)
    assert(repo.idToFileBased == Map(
      (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
      (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1)),
      (BWorkflowPath % V3) → None,
      (CWorkflowPath % V2) → Some(TestWorkflow.withId(CWorkflowPath % V2))))
  }

  "updateMasterConfiguration without VersionId" in {
    writeFile(DWorkflowPath)
    readConfiguration(None).orThrow
    assert(repo.currentTyped[Workflow].apply(DWorkflowPath).id.versionId.string startsWith "#")  // Master has generated a VersionId
  }

  private def readConfiguration(versionId: Option[VersionId]): Checked[Response.Accepted] =
    AkkaHttpClient.liftProblem(master.httpApi.executeCommand(ReadConfigurationDirectory(versionId)))
      .await(99.seconds)


  private def repo: Repo = master.fileBasedApi.stampedRepo.await(99.seconds).value

  private def writeFile(workflowPath: WorkflowPath): Unit =
    live.resolve(workflowPath.toFile(SourceType.Json)) := TestWorkflowJson
}

object ReadConfigurationDirectoryTest
{
  private val userAndPassword = UserAndPassword(UserId("ReadConfigurationDirectoryTest"), SecretString("PASSWORD"))

  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val DWorkflowPath = WorkflowPath("/D")

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
}
