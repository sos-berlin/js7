package com.sos.jobscheduler.master.repo

import com.sos.jobscheduler.base.auth.User.UserDoesNotHavePermissionProblem
import com.sos.jobscheduler.base.auth.{SimpleUser, UpdateRepoPermission, UserId}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, FileBasedVerifier, Repo}
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.filebased.{FileBased, VersionId}
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.workflow.instructions.Fail
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoCommandExecutorTest extends AnyFreeSpec
{
  private lazy val signer = new SillySigner(SillySignature("RepoCommandExecutorTest"))
  private lazy val signatureVerifier = signer.toVerifier
  private lazy val fileBasedVerifier = new FileBasedVerifier[FileBased](signatureVerifier, MasterFileBaseds.jsonCodec)
  private lazy val fileBasedSigner = new FileBasedSigner[FileBased](signer, MasterFileBaseds.jsonCodec)
  private lazy val repoCommandExecutor = new RepoCommandExecutor(fileBasedVerifier)
  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val v3 = VersionId("3")
  private val agentRef1 = AgentRef(AgentRefPath("/AGENT") ~ v1, Uri("https://example.com"))
  private val workflow2 = Workflow(WorkflowPath("/WORKFLOW") ~ v2, Vector(Fail(None)))
  private val workflow3 = workflow2 withVersion v3
  private val commandMeta = CommandMeta(SimpleUser(UserId("PROVIDER")).copy(grantedPermissions = Set(UpdateRepoPermission)))

  private var repo = Repo(MasterFileBaseds.jsonCodec)

  "replaceRepoCommandToEvents requires UpdateRepo permission" in {
    val commandMeta = CommandMeta(SimpleUser(UserId("HACKER")))
    assert(repoCommandExecutor.replaceRepoCommandToEvents(repo, ReplaceRepo(v1, Nil), commandMeta)
      == Left(UserDoesNotHavePermissionProblem(UserId("HACKER"), UpdateRepoPermission)))
  }

  "updateRepoCommandToEvents requires UpdateRepo permission" in {
    val commandMeta = CommandMeta(SimpleUser(UserId("HACKER")))
    assert(repoCommandExecutor.updateRepoCommandToEvents(repo, UpdateRepo(v1), commandMeta)
      == Left(UserDoesNotHavePermissionProblem(UserId("HACKER"), UpdateRepoPermission)))
  }

  "replaceRepoCommandToEvents" in {
    repo = executeReplace(ReplaceRepo(v1, fileBasedSigner.sign(agentRef1) :: Nil))
    assert(repo.idToSignedFileBased == Map(
      agentRef1.id -> Some(fileBasedSigner.toSigned(agentRef1))))
  }

  "updateRepoCommandToEvents" in {
    repo = executeUpdate(UpdateRepo(v2, fileBasedSigner.sign(workflow2) :: Nil))
    assert(repo.idToSignedFileBased == Map(
      agentRef1.id -> Some(fileBasedSigner.toSigned(agentRef1)),
      workflow2.id -> Some(fileBasedSigner.toSigned(workflow2))))
  }

  "replaceRepoCommandToEvents #2" in {
    repo = executeReplace(ReplaceRepo(v3, fileBasedSigner.sign(workflow3) :: Nil))
    assert(repo.idToSignedFileBased == Map(
      agentRef1.id -> Some(fileBasedSigner.toSigned(agentRef1)),
      agentRef1.path ~ v3 -> None,
      workflow2.id -> Some(fileBasedSigner.toSigned(workflow2)),
      workflow2.path ~ v3 -> None,
      workflow3.id -> Some(fileBasedSigner.toSigned(workflow3))))
  }

  private def executeReplace(replaceRepo: ReplaceRepo): Repo =
    repo.applyEvents(repoCommandExecutor.replaceRepoCommandToEvents(repo, replaceRepo, commandMeta).orThrow).orThrow

  private def executeUpdate(updateRepo: UpdateRepo): Repo =
    repo.applyEvents(repoCommandExecutor.updateRepoCommandToEvents(repo, updateRepo, commandMeta).orThrow).orThrow
}
