package js7.data.controller

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{SimpleUser, UpdateItemPermission, UserId, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.crypt.x509.X509Signer
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.crypt.SignedItemVerifier
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemSigner, SignableItem, VersionId, VersionedItem}
import js7.data.lock.{Lock, LockPath}
import js7.data.workflow.instructions.Fail
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class VerifiedUpdateItemsTest extends AnyFreeSpec
{
  private lazy val (signer, signatureVerifier) = X509Signer.forTest
  private lazy val itemVerifier = new SignedItemVerifier(signatureVerifier, signableItemJsonCodec)
  private lazy val itemSigner = new ItemSigner[SignableItem](signer, signableItemJsonCodec)
  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val workflow1 = Workflow(WorkflowPath("WORKFLOW-A") ~ v1, Vector(Fail(None)))
  private val workflow2 = Workflow(WorkflowPath("WORKFLOW") ~ v2, Vector(Fail(None)))
  private val lock = Lock(LockPath("LOCK-1"))
  private val user = SimpleUser(UserId("PROVIDER")).copy(grantedPermissions = Set(ValidUserPermission, UpdateItemPermission))
  private def noVerifier(signedString: SignedString): Checked[Verified[SignableItem]] = Left(Problem("NO VERIFIER"))

  "UpdateItemPermission is required" in {
    val user = SimpleUser(UserId("TESTER"), grantedPermissions = Set(ValidUserPermission))
    assert(VerifiedUpdateItems.fromOperations(Observable.empty, noVerifier, user).await(99.s) ==
      Left(UserDoesNotHavePermissionProblem(user.id, UpdateItemPermission)))
  }

  "Simple items only" in {
    // TODO Test SignableSimpleItem
    VerifiedUpdateItems.fromOperations(Observable(AddOrChangeSimple(lock), DeleteSimple(LockPath("DELETE"))), noVerifier, user).await(99.s) ==
      Right(VerifiedUpdateItems(
        VerifiedUpdateItems.Simple(Seq(lock), Nil, delete = Nil),
        maybeVersioned = None))
  }

  "Verification" in {
    val operations = Observable(
      AddOrChangeSimple(lock),
      DeleteSimple(LockPath("DELETE")),
      AddVersion(v1),
      AddOrChangeSigned(itemSigner.toSignedString(workflow1)),
      RemoveVersioned(WorkflowPath("DELETE")))
    assert(VerifiedUpdateItems.fromOperations(operations, itemVerifier.verify, user).await(99.s) ==
      Right(VerifiedUpdateItems(
        VerifiedUpdateItems.Simple(Seq(lock), Nil, delete = Seq(LockPath("DELETE"))),
        Some(VerifiedUpdateItems.Versioned(
          v1,
          Seq(
            itemVerifier.verify(itemSigner.toSignedString(workflow1))
              .orThrow
              .asInstanceOf[Verified[VersionedItem]]),
          Seq(WorkflowPath("DELETE")))))))
  }

  "Verification failed" in {
    val wrongSignature = itemSigner.toSignedString(workflow2).signature
    val operations = Observable(
      AddVersion(v1),
      AddOrChangeSigned(itemSigner.toSignedString(workflow1).copy(signature = wrongSignature)))
    assert(VerifiedUpdateItems.fromOperations(operations, itemVerifier.verify, user).await(99.s) ==
      Left(TamperedWithSignedMessageProblem))
  }

  "Duplicate SimpleItems are rejected" in {
    assert(
      VerifiedUpdateItems.fromOperations(Observable(AddOrChangeSimple(lock), AddOrChangeSimple(lock)), noVerifier, user).await(99.s) ==
        Left(Problem("Unexpected duplicates: 2×Lock:LOCK-1")))
    assert(
      VerifiedUpdateItems.fromOperations(Observable(AddOrChangeSimple(lock), DeleteSimple(lock.path)), noVerifier, user).await(99.s) ==
        Left(Problem("Unexpected duplicates: 2×Lock:LOCK-1")))
  }

  "Duplicate VersionedItems are rejected" in {
    assert(
      VerifiedUpdateItems.fromOperations(
        Observable(
          AddVersion(v1),
          AddOrChangeSigned(itemSigner.toSignedString(workflow1)),
          AddOrChangeSigned(itemSigner.toSignedString(Workflow.of(workflow1.id)))),
        itemVerifier.verify,
        user
      ).await(99.s) ==
        Left(Problem("Unexpected duplicates: 2×Workflow:WORKFLOW-A")))

    assert(
      VerifiedUpdateItems.fromOperations(
        Observable(
          AddVersion(v1),
          AddOrChangeSigned(itemSigner.toSignedString(workflow1)),
          RemoveVersioned(workflow1.path)),
        itemVerifier.verify,
        user
      ).await(99.s) ==
        Left(Problem("Unexpected duplicates: 2×Workflow:WORKFLOW-A")))
  }

  "Duplicate AddVersion is rejected" in {
    assert(
      VerifiedUpdateItems.fromOperations(
        Observable(AddVersion(v1), AddVersion(v1)), itemVerifier.verify, user).await(99.s) ==
        Left(Problem("Duplicate AddVersion")))

    assert(
      VerifiedUpdateItems.fromOperations(
        Observable(AddVersion(v1), AddVersion(v2)), itemVerifier.verify, user).await(99.s) ==
        Left(Problem("Duplicate AddVersion")))
  }
}
