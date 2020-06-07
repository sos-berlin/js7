package js7.data.filebased

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.time.Stopwatch
import js7.data.agent.AgentRefPath
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.Repo.testOnly.{Changed, Deleted, OpRepo}
import js7.data.filebased.Repo.{EventVersionDoesNotMatchProblem, FileBasedDeletedProblem, ObjectVersionDoesNotMatchProblem}
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import js7.data.filebased.RepoTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends AnyFreeSpec
{
  import fileBasedSigner.{sign, toSigned}

  private lazy val Right(testRepo) = emptyRepo.applyEvents(TestEvents)

  "empty" in {
    assert(emptyRepo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(emptyRepo.idTo[AFileBased](APath("/UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("TypedPath", APath("/UNKNOWN-PATH"))))

    assert(emptyRepo.applyEvent(FileBasedAdded(a1.path, SignedString(a1.asJson.compactPrint, GenericSignature("SILLY", "SIGNED")))) ==
      Left(Problem("Missing initial VersionAdded event for Repo")))

    assert(!emptyRepo.exists(APath("/A")))
  }

  "empty version" in {
    val Right(repo) = emptyRepo.applyEvent(VersionAdded(v("INITIAL")))
    assert(repo.historyBefore(v("INITIAL")) == Right(Nil))
    assert(repo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(repo.idTo[AFileBased](APath("/UNKNOWN") ~ "INITIAL") ==
      Left(UnknownKeyProblem("TypedPath", APath("/UNKNOWN"))))

    assert(repo.idTo[AFileBased](APath("/UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("TypedPath", APath("/UNKNOWN-PATH"))))

    assert(repo.applyEvent(VersionAdded(v("INITIAL"))) ==
      Left(DuplicateKey("VersionId", v("INITIAL"))))
  }

  "exists" in {
    assert(testRepo.exists(APath("/A")))
    assert(!testRepo.exists(APath("/Bx")))
    assert(!testRepo.exists(APath("/UNKNOWN")))
  }

  "Event input" in {
    assert(testRepo.idTo[AFileBased](APath("/A") ~ "UNKNOWN") == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))
    assert(testRepo.idTo[AFileBased](APath("/X") ~ V1) == Left(UnknownKeyProblem("TypedPath", APath("/X"))))
    assert(testRepo.idTo[AFileBased](APath("/X") ~ V1) == Left(UnknownKeyProblem("TypedPath", APath("/X"))))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") ~ V1) == Left(UnknownKeyProblem("FileBasedId", BPath("/Bx") ~ V1)))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") ~ V3) == Left(FileBasedDeletedProblem(BPath("/Bx") ~ V3)))
    assert(testRepo.idTo[AFileBased](APath("/A") ~ V1) == Right(a1))
    assert(testRepo.idTo[AFileBased](APath("/A") ~ V2) == Right(a2))
    assert(testRepo.idTo[AFileBased](APath("/A") ~ V3) == Right(a3))
    assert(testRepo.idTo[BFileBased](BPath("/Bx") ~ V2) == Right(bx2))
    assert(testRepo.idTo[BFileBased](BPath("/By") ~ V3) == Right(by2))
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

  //"pathToCurrentId" in {
  //  assert(testRepo.pathToCurrentId(APath("/A")) == Right(APath("/A") ~ V3))
  //  assert(testRepo.pathToCurrentId(BPath("/A")) == Left(Problem("No such key 'B:/A'")))
  //  assert(testRepo.pathToCurrentId(BPath("/Bx")) == Left(Problem("Has been deleted: B:/Bx")))
  //  assert(testRepo.pathToCurrentId(BPath("/By")) == Right(BPath("/By") ~ V2))
  //  assert(testRepo.pathToCurrentId(APath("/X")) == Left(Problem("No such AFileBased: A:/X")))
  //}

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
    for (_ <- 1 to n) {
      repo = repo.applyEvent(VersionAdded(repo.newVersionId())).orThrow
    }
    assert(repo.versions.size - testRepo.versions.size == n)
    assert(repo.versions.toSet.size - testRepo.versions.size == n)
  }

  "idTo" in {
    assert(testRepo.idTo[AFileBased](a1.id) == Right(a1))
  }

  "typedCount" in {
    assert(testRepo.typedCount[AFileBased] == 3)
    assert(testRepo.typedCount[BFileBased] == 2)
  }

  "currentTyped" in {
    assert(testRepo.currentTyped[AFileBased] == Map(a3.path -> a3))
    assert(testRepo.currentTyped[BFileBased] == Map(by2.path -> by2))
  }

  "toEvent" - {
    "FileBased with alien version is rejected" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1.withVersion(V2)) :: Nil) == Left(ObjectVersionDoesNotMatchProblem(VersionId("1"), a1.path ~ V2)))
    }

    "FileBased without version is rejected" in {
      // The signer signs the VersionId, too. It must not be diverge from the commands VersionId
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1.withoutVersion)  :: Nil) == Left(ObjectVersionDoesNotMatchProblem(VersionId("1"), a1.path)))
    }

    "FileBased with matching version" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: Nil)
        == Right(VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) :: Nil))
    }

    "Deleting unknown" in {
      assert(emptyRepo.fileBasedToEvents(V1, Nil, deleted = bx2.path :: Nil)
        == Right(VersionAdded(V1) :: Nil))
    }

    "Duplicate" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(a1) :: Nil)
        == Left(Problem("Unexpected duplicates: A:/A")))
    }

    "Other" in {
      assert(emptyRepo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil, deleted = bx2.path :: Nil)
        == Right(VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) :: FileBasedAdded(b1.path, sign(b1)) :: Nil))
    }

    "More" in {
      var repo = emptyRepo
      repo = repo.fileBasedToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil).flatMap(repo.applyEvents).orThrow
      assert(repo == Repo.fromOp(V1 :: Nil, Changed(toSigned(a1)) :: Changed(toSigned(b1)) :: Nil, fileBasedVerifier))

      val events = repo.fileBasedToEvents(V2, toSigned(a2) :: toSigned(bx2) :: Nil, deleted = b1.path :: Nil).orThrow
      assert(events == VersionAdded(V2) :: FileBasedDeleted(b1.path) :: FileBasedChanged(a2.path, sign(a2)) :: FileBasedAdded(bx2.path, sign(bx2)) :: Nil)

      repo = repo.applyEvents(events).orThrow
      assert(repo == Repo.fromOp(V2 :: V1 :: Nil,
        Changed(toSigned(a1)) :: Changed(toSigned(a2)) :: Changed(toSigned(b1)) :: Deleted(b1.path ~ V2) :: Changed(toSigned(bx2)) :: Nil,
        fileBasedVerifier))

      assert(repo.applyEvents(events) == Left(DuplicateKey("VersionId", VersionId("2"))))
    }
  }

  "applyEvent" - {
    var repo = emptyRepo.applyEvent(VersionAdded(V1)).orThrow

    "FileBasedChanged for unknown path" in {
      assert(repo.applyEvent(FileBasedChanged(APath("/A"), sign(AFileBased(APath("/A") ~ V1, "A")))) ==
        Left(UnknownKeyProblem("TypedPath", APath("/A"))))
    }

    "FileBasedDeleted for unknown path" in {
      assert(repo.applyEvent(FileBasedDeleted(APath("/A"))) ==
        Left(UnknownKeyProblem("TypedPath", APath("/A"))))
    }

    "FileBasedAdded for existent path" in {
      repo = repo.applyEvent(FileBasedAdded(APath("/A"), sign(AFileBased(APath("/A") ~ V1, "A")))).orThrow
      assert(repo.applyEvent(FileBasedAdded(APath("/A"), sign(AFileBased(APath("/A") ~ V1, "A")))) ==
        Left(DuplicateKey("TypedPath", APath("/A"))))
    }

    "FileBaseAdded with different VersionId" in {
      val event = FileBasedAdded(APath("/B"), sign(AFileBased(APath("/B") ~ V3, "B")))
      assert(repo.applyEvent(event) ==
        Left(EventVersionDoesNotMatchProblem(V1, event)))
    }

    "FileBaseChanged with different VersionId" in {
      val event = FileBasedChanged(APath("/A"), sign(AFileBased(APath("/A") ~ V3, "A")))
      assert(repo.applyEvent(event) == Left(EventVersionDoesNotMatchProblem(V1, event)))
    }

    "FileBasedChanged for deleted path" in {
      repo = repo.applyEvent(VersionAdded(V2)).orThrow
      repo = repo.applyEvent(FileBasedDeleted(APath("/A"))).orThrow
      repo = repo.applyEvent(VersionAdded(V3)).orThrow
      assert(repo.applyEvent(FileBasedChanged(APath("/A"), sign(AFileBased(APath("/A") ~ V3, "A")))) ==
        Left(FileBasedDeletedProblem(APath("/A") ~ V2)))
    }

    "FileBasedDeleted for deleted path" in {
      assert(repo.applyEvent(FileBasedDeleted(APath("/A"))) == Left(FileBasedDeletedProblem(APath("/A") ~ V2)))
    }
  }

  locally {
    val n = sys.props.get("RepoTest").map(_.toInt) getOrElse 2000
    s"Add many $n versions" in {
      // MasterCommand.UpdateRepo calls fileBasedToEvents
      var repo = emptyRepo
      val stopwatch = new Stopwatch
      var sw = new Stopwatch
      for (i <- 1 to n) {
        val v = VersionId(i.toString)
        val events = repo.fileBasedToEvents(v, toSigned(AFileBased(APath(s"/A-$i"), "A") withVersion v) :: Nil).orThrow
        repo = repo.applyEvents(events).orThrow
        if (i % 1000 == 0) {
          scribe.info(sw.itemsPerSecondString(1000, "versions"))
          sw = new Stopwatch
        }
      }
      info(stopwatch.itemsPerSecondString(n, "versions"))
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

object RepoTest
{
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val a1 = AFileBased(APath("/A") ~ V1, "A-1")
  private val a2 = AFileBased(APath("/A") ~ V2, "A-2")
  private val b1 = BFileBased(BPath("/B") ~ V1, "B-1")
  private val bx2 = BFileBased(BPath("/Bx") ~ V2, "Ba-2")
  private val by2 = BFileBased(BPath("/By") ~ V2, "Bb-2")
  private val a3 = AFileBased(APath("/A") ~ V3, "A-3")

  private implicit val fileBasedJsonCodec = TypedJsonCodec[FileBased](
    Subtype[AFileBased],
    Subtype[BFileBased])

  private val fileBasedSigner = new FileBasedSigner(SillySigner.Default, fileBasedJsonCodec)
  private val fileBasedVerifier = new FileBasedVerifier(new SillySignatureVerifier, fileBasedJsonCodec)
  private val emptyRepo = Repo.signatureVerifying(fileBasedVerifier)

  import fileBasedSigner.sign

  private val TestEvents =
    VersionAdded(V1) :: FileBasedAdded(a1.path, sign(a1)) ::
    VersionAdded(V2) :: FileBasedChanged(a2.path, sign(a2)) :: FileBasedAdded(bx2.path, sign(bx2)) :: FileBasedAdded(by2.path, sign(by2)) ::
    VersionAdded(V3) :: FileBasedChanged(a3.path, sign(a3)) :: FileBasedDeleted(bx2.path) :: Nil

  private def v(version: String) = VersionId(version)
}
