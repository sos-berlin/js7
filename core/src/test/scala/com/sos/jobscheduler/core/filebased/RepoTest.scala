package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.crypt.silly.{SillySignatureVerifier, SillySigner}
import com.sos.jobscheduler.core.filebased.Repo.{Changed, Deleted}
import com.sos.jobscheduler.core.filebased.RepoTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignedString}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{AFileBased, APath, BFileBased, BPath, FileBased, VersionId}
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends FreeSpec
{
  import fileBasedSigner.{sign, toSigned}

  private lazy val Valid(testRepo) = emptyRepo.applyEvents(TestEvents)

  "empty" in {
    assert(emptyRepo.historyBefore(v("UNKNOWN")) == Invalid(Problem("No such 'version UNKNOWN'")))

    assert(emptyRepo.idTo[AFileBased](APath("/UNKNOWN-PATH") % "UNKNOWN-VERSION") ==
      Invalid(Problem("No such key 'A:/UNKNOWN-PATH'")))

    assert(emptyRepo.applyEvent(FileBasedAdded(a1.path, SignedString(a1.asJson.compactPrint, GenericSignature("SILLY", "SIGNED")))) ==
      Invalid(Problem("Missing first event VersionAdded for Repo")))
  }

  "empty version" in {
    val Valid(repo) = emptyRepo.applyEvent(VersionAdded(v("INITIAL")))
    assert(repo.historyBefore(v("INITIAL")) == Valid(Nil))
    assert(repo.historyBefore(v("UNKNOWN")) == Invalid(Problem("No such 'version UNKNOWN'")))

    assert(repo.idTo[AFileBased](APath("/UNKNOWN") % "INITIAL") ==
      Invalid(Problem("No such key 'A:/UNKNOWN'")))

    assert(repo.idTo[AFileBased](APath("/UNKNOWN-PATH") % "UNKNOWN-VERSION") ==
      Invalid(Problem("No such key 'A:/UNKNOWN-PATH'")))

    assert(repo.applyEvent(VersionAdded(v("INITIAL"))) ==
      Invalid(Repo.DuplicateVersionProblem(v("INITIAL"))))
  }

  "Event input" in {
    assert(testRepo.idTo[AFileBased](APath("/A") % "4") == Invalid(Problem("No such 'version 4'")))
    assert(testRepo.idTo[AFileBased](APath("/X") % V1) == Invalid(Problem("No such key 'A:/X'")))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") % V1) == Invalid(Problem("No such 'B:/Bx 1'")))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") % V3) == Invalid(Problem("Has been deleted: B:/Bx 3")))
    assert(testRepo.idTo[AFileBased](APath("/A") % V1) == Valid(a1))
    assert(testRepo.idTo[AFileBased](APath("/A") % V2) == Valid(a2))
    assert(testRepo.idTo[AFileBased](APath("/A") % V3) == Valid(a3))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") % V2) == Valid(bx2))
    assert(testRepo.idTo[BFileBased](BPath("/By") % V3) == Valid(by2))
  }

  "Event output" in {
    assert(testRepo.toEvents == TestEvents)
  }

  "eventsFor" in {
    assert(testRepo.eventsFor(Set(APath, BPath)) == TestEvents)
    assert(testRepo.eventsFor(Set(APath)) == List(
      VersionAdded(V1), FileBasedAdded(a1.path, sign(a1)),
      VersionAdded(V2), FileBasedChanged(a2.path, sign(a2)),
      VersionAdded(V3), FileBasedChanged(a3.path, sign(a3))))
    assert(testRepo.eventsFor(Set(AgentRefPath)) == List(VersionAdded(V1), VersionAdded(V2), VersionAdded(V3)))
  }

  "pathToCurrentId" in {
    assert(testRepo.pathToCurrentId(APath("/A")) == Valid(APath("/A") % V3))
    assert(testRepo.pathToCurrentId(BPath("/A")) == Invalid(Problem("No such key 'B:/A'")))
    assert(testRepo.pathToCurrentId(BPath("/Bx")) == Invalid(Problem("Has been deleted: B:/Bx")))
    assert(testRepo.pathToCurrentId(BPath("/By")) == Valid(BPath("/By") % V2))
    assert(testRepo.pathToCurrentId(APath("/X")) == Invalid(Problem("No such key 'A:/X'")))
  }

  "currentVersion" in {
    assert(testRepo.currentVersion == Map(
      a3.path -> toSigned(a3),
      by2.path -> toSigned(by2)))
  }

  "versionId" in {
    assert(testRepo.versionId == V3)
    assert(emptyRepo.versionId.isAnonymous)
  }

  "newVersionId returns unique value" in {
    var repo = testRepo
    val n = 100
    for (_ ← 1 to n) {
      repo = repo.applyEvent(VersionAdded(repo.newVersionId())).orThrow
    }
    assert(repo.versions.size - testRepo.versions.size == n)
    assert(repo.versions.toSet.size - testRepo.versions.size == n)
  }

  "idTo" in {
    assert(testRepo.idTo[AFileBased](a1.id) == Valid(a1))
  }

  "typedCount" in {
    assert(testRepo.typedCount[AFileBased] == 3)
    assert(testRepo.typedCount[BFileBased] == 2)
  }

  "currentTyped" in {
    assert(testRepo.currentTyped[AFileBased] == Map(a3.path → a3))
    assert(testRepo.currentTyped[BFileBased] == Map(by2.path → by2))
  }

  "toEvent" - {
    "FileBased with alien version is rejected" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1.withVersion(V2)) :: Nil) == Invalid(Problem("Expected version '1' in 'A:/A 2'")))
    }

    "FileBased without version is rejected" in {
      // The signer signs the VersionId, too. It must not be diverge from the commands VersionId
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1.withoutVersion)  :: Nil) == Invalid(Problem("Expected version '1' in 'A:/A'")))
    }

    "FileBased with matching version" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: Nil)
        == Valid(VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) :: Nil))
    }

    "Deleting unknown" in {
      assert(emptyRepo.fileBasedToEvents(V1, Nil, deleted = bx2.path :: Nil)
        == Valid(VersionAdded(V1) :: Nil))
    }

    "Duplicate" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(a1) :: Nil)
        == Invalid(Problem("Unexpected duplicates: A:/A")))
    }

    "Other" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil, deleted = bx2.path :: Nil)
        == Valid(VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) :: FileBasedAdded(b1.path, sign(b1)) :: Nil))
    }

    "More" in {
      var repo = emptyRepo
      repo = repo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil).flatMap(repo.applyEvents).orThrow
      assert(repo == Repo(V1 :: Nil, Changed(toSigned(a1)) :: Changed(toSigned(b1)) :: Nil, fileBasedVerifier))

      val events = repo.fileBasedToEvents(V2, toSigned(a2) :: toSigned(bx2) :: Nil, deleted = b1.path :: Nil).orThrow
      assert(events == VersionAdded(V2) :: FileBasedDeleted(b1.path) :: FileBasedChanged(a2.path, sign(a2)) :: FileBasedAdded(bx2.path, sign(bx2)) :: Nil)

      repo = repo.applyEvents(events).orThrow
      assert(repo == Repo(V2 :: V1 :: Nil, Changed(toSigned(a1)) :: Changed(toSigned(a2)) :: Changed(toSigned(b1)) :: Deleted(b1.path % V2) :: Changed(toSigned(bx2)) :: Nil, fileBasedVerifier))

      assert(repo.applyEvents(events) == Invalid(Problem("Duplicate VersionId '2'")))
    }
  }

  //"diff" in {
  //  // NOT USED ?
  //  assert(Repo.diff(a1 :: b1  :: Nil, a1 :: b1  :: Nil) == Nil)
  //  assert(Repo.diff(a1 :: b1  :: Nil,              Nil) == FileBasedAdded(a1.withoutVersion) :: FileBasedAdded(b1.withoutVersion) :: Nil)
  //  assert(Repo.diff(a1 :: b1  :: Nil, a1 ::        Nil) == FileBasedAdded(b1.withoutVersion) :: Nil)
  //  assert(Repo.diff(             Nil, a1 :: b1  :: Nil) == FileBasedDeleted(a1.path) :: FileBasedDeleted(b1.path) :: Nil)
  //  assert(Repo.diff(a1 ::        Nil, a1 :: b1  :: Nil) == FileBasedDeleted(b1.path) :: Nil)
  //  assert(Repo.diff(a1 :: by2 :: Nil, a1 :: bx2 :: Nil) == FileBasedDeleted(bx2.path) :: FileBasedAdded(by2.withoutVersion) :: Nil)
  //}

}

object RepoTest {
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val a1 = AFileBased(APath("/A") % V1, "A-1")
  private val a2 = AFileBased(APath("/A") % V2, "A-2")
  private val b1 = BFileBased(BPath("/B") % V1, "B-1")
  private val bx2 = BFileBased(BPath("/Bx") % V2, "Ba-2")
  private val by2 = BFileBased(BPath("/By") % V2, "Bb-2")
  private val a3 = AFileBased(APath("/A") % V3, "A-3")

  private implicit val fileBasedJsonCodec = TypedJsonCodec[FileBased](
    Subtype[AFileBased],
    Subtype[BFileBased])

  private val fileBasedSigner = new FileBasedSigner(new SillySigner, fileBasedJsonCodec)
  private val fileBasedVerifier = new FileBasedVerifier(new SillySignatureVerifier, fileBasedJsonCodec)
  private val emptyRepo = Repo.signatureVerifying(fileBasedVerifier)

  import fileBasedSigner.sign

  private val TestEvents =
    VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) ::
    VersionAdded(V2) :: FileBasedChanged(a2.path, sign(a2)) :: FileBasedAdded(bx2.path, sign(bx2)) :: FileBasedAdded(by2.path, sign(by2)) ::
    VersionAdded(V3) :: FileBasedChanged(a3.path, sign(a3)) :: FileBasedDeleted(bx2.path) :: Nil

  private def v(version: String) = VersionId(version)
}
