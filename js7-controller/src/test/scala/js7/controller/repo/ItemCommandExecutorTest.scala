package js7.controller.repo

import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{SimpleUser, UpdateItemPermission, UserId}
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.{ReplaceRepo, UpdateRepo}
import js7.controller.data.ControllerState.versionedItemJsonCodec
import js7.controller.item.ItemCommandExecutor
import js7.core.command.CommandMeta
import js7.data.crypt.VersionedItemVerifier
import js7.data.item.Repo.Entry
import js7.data.item.{Repo, VersionId, VersionedItem, VersionedItemSigner}
import js7.data.workflow.instructions.Fail
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ItemCommandExecutorTest extends AnyFreeSpec
{
  private lazy val signer = new SillySigner(SillySignature("ItemCommandExecutorTest"))
  private lazy val signatureVerifier = signer.toVerifier
  private lazy val itemVerifier = new VersionedItemVerifier[VersionedItem](signatureVerifier, versionedItemJsonCodec)
  private lazy val itemSigner = new VersionedItemSigner[VersionedItem](signer, versionedItemJsonCodec)
  private lazy val repoCommandExecutor = new ItemCommandExecutor(itemVerifier)
  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val v3 = VersionId("3")
  private val workflow1 = Workflow(WorkflowPath("/WORKFLOW-A") ~ v1, Vector(Fail(None)))
  private val workflow2 = Workflow(WorkflowPath("/WORKFLOW") ~ v2, Vector(Fail(None)))
  private val workflow3 = workflow2 withVersion v3
  private val commandMeta = CommandMeta(SimpleUser(UserId("PROVIDER")).copy(grantedPermissions = Set(UpdateItemPermission)))

  private var repo = Repo.empty

  "replaceRepoCommandToEvents requires UpdateItemPermission" in {
    val commandMeta = CommandMeta(SimpleUser(UserId("HACKER")))
    assert(repoCommandExecutor.replaceRepoCommandToEvents(repo, ReplaceRepo(v1, Nil), commandMeta).await(99.s)
      == Left(UserDoesNotHavePermissionProblem(UserId("HACKER"), UpdateItemPermission)))
  }

  "updateRepoCommandToEvents requires UpdateItemPermission" in {
    val commandMeta = CommandMeta(SimpleUser(UserId("HACKER")))
    assert(repoCommandExecutor.updateRepoCommandToEvents(repo, UpdateRepo(v1), commandMeta).await(99.s)
      == Left(UserDoesNotHavePermissionProblem(UserId("HACKER"), UpdateItemPermission)))
  }

  "replaceRepoCommandToEvents" in {
    repo = executeReplace(ReplaceRepo(v1, itemSigner.sign(workflow1) :: Nil))
    assert(repo.pathToVersionToSignedItems == Map(
      workflow1.path -> List(Entry(workflow1.id.versionId, Some(itemSigner.toSigned(workflow1))))))
  }

  "updateRepoCommandToEvents" in {
    repo = executeUpdate(UpdateRepo(v2, itemSigner.sign(workflow2) :: Nil))
    assert(repo.pathToVersionToSignedItems == Map(
      workflow1.path -> List(Entry(workflow1.id.versionId, Some(itemSigner.toSigned(workflow1)))),
      workflow2.path -> List(Entry(workflow2.id.versionId, Some(itemSigner.toSigned(workflow2))))))
  }

  "replaceRepoCommandToEvents #2" in {
    repo = executeReplace(ReplaceRepo(v3, itemSigner.sign(workflow3) :: Nil))
    assert(repo.pathToVersionToSignedItems == Map(
      workflow1.path -> List(
        Entry(v3, None),
        Entry(v1, Some(itemSigner.toSigned(workflow1)))),
      workflow2.path -> List(
        Entry(v3, Some(itemSigner.toSigned(workflow3))),
        Entry(v2, Some(itemSigner.toSigned(workflow2))))))
  }

  private def executeReplace(replaceRepo: ReplaceRepo): Repo =
    repo.applyEvents(repoCommandExecutor.replaceRepoCommandToEvents(repo, replaceRepo, commandMeta).await(99.s).orThrow)
      .orThrow

  private def executeUpdate(updateRepo: UpdateRepo): Repo =
    repo.applyEvents(repoCommandExecutor.updateRepoCommandToEvents(repo, updateRepo, commandMeta).await(99.s).orThrow)
      .orThrow
}
