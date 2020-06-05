package js7.master.repo

import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{SimpleUser, UpdateRepoPermission, UserId}
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.problem.Checked.Ops
import js7.base.web.Uri
import js7.core.command.CommandMeta
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.Repo.Entry
import js7.data.filebased.{FileBased, FileBasedSigner, Repo, VersionId}
import js7.data.master.MasterFileBaseds
import js7.data.workflow.instructions.Fail
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
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

  private var repo = Repo.ofJsonDecoder(MasterFileBaseds.jsonCodec)

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
    assert(repo.pathToVersionToSignedFileBased == Map(
      agentRef1.id.path -> List(Entry(agentRef1.id.versionId, Some(fileBasedSigner.toSigned(agentRef1))))))
  }

  "updateRepoCommandToEvents" in {
    repo = executeUpdate(UpdateRepo(v2, fileBasedSigner.sign(workflow2) :: Nil))
    assert(repo.pathToVersionToSignedFileBased == Map(
      agentRef1.id.path -> List(Entry(agentRef1.id.versionId, Some(fileBasedSigner.toSigned(agentRef1)))),
      workflow2.id.path -> List(Entry(workflow2.id.versionId, Some(fileBasedSigner.toSigned(workflow2))))))
  }

  "replaceRepoCommandToEvents #2" in {
    repo = executeReplace(ReplaceRepo(v3, fileBasedSigner.sign(workflow3) :: Nil))
    assert(repo.pathToVersionToSignedFileBased == Map(
      agentRef1.id.path -> List(
        Entry(v3, None),
        Entry(v1, Some(fileBasedSigner.toSigned(agentRef1)))),
      workflow2.id.path -> List(
        Entry(v3, Some(fileBasedSigner.toSigned(workflow3))),
        Entry(v2, Some(fileBasedSigner.toSigned(workflow2))))))
  }

  private def executeReplace(replaceRepo: ReplaceRepo): Repo =
    repo.applyEvents(repoCommandExecutor.replaceRepoCommandToEvents(repo, replaceRepo, commandMeta).orThrow).orThrow

  private def executeUpdate(updateRepo: UpdateRepo): Repo =
    repo.applyEvents(repoCommandExecutor.updateRepoCommandToEvents(repo, updateRepo, commandMeta).orThrow).orThrow
}
