package js7.data.item

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.{GenericSignature, Signed, SignedString}
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.time.Stopwatch
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemVersionDoesNotMatchProblem, VersionedItemDeletedProblem}
import js7.data.agent.AgentId
import js7.data.crypt.VersionedItemVerifier
import js7.data.item.Repo.testOnly.{Changed, Deleted, OpRepo}
import js7.data.item.RepoTest._
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged, VersionedItemDeleted}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends AnyFreeSpec
{
  import itemSigner.toSigned

  private lazy val Right(testRepo) = emptyRepo.applyEvents(TestEvents)

  "empty" in {
    assert(emptyRepo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(emptyRepo.idTo[AItem](APath("UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("ItemPath", APath("UNKNOWN-PATH"))))

    assert(emptyRepo.applyEvent(VersionedItemAdded(Signed(a1, SignedString(a1.asJson.compactPrint, GenericSignature("SILLY", "SIGNED"))))) ==
      Left(Problem("Missing initial VersionAdded event for Repo")))

    assert(!emptyRepo.exists(APath("A")))
  }

  "empty version" in {
    val Right(repo) = emptyRepo.applyEvent(VersionAdded(v("INITIAL")))
    assert(repo.historyBefore(v("INITIAL")) == Right(Nil))
    assert(repo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(repo.idTo[AItem](APath("UNKNOWN") ~ "INITIAL") ==
      Left(UnknownKeyProblem("ItemPath", APath("UNKNOWN"))))

    assert(repo.idTo[AItem](APath("UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("ItemPath", APath("UNKNOWN-PATH"))))

    assert(repo.applyEvent(VersionAdded(v("INITIAL"))) ==
      Left(DuplicateKey("VersionId", v("INITIAL"))))
  }

  "exists" in {
    assert(testRepo.exists(APath("A")))
    assert(!testRepo.exists(APath("Bx")))
    assert(!testRepo.exists(APath("UNKNOWN")))
  }

  "Event input" in {
    assert(testRepo.idTo[AItem](APath("A") ~ "UNKNOWN") == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))
    assert(testRepo.idTo[AItem](APath("X") ~ V1) == Left(UnknownKeyProblem("ItemPath", APath("X"))))
    assert(testRepo.idTo[AItem](APath("X") ~ V1) == Left(UnknownKeyProblem("ItemPath", APath("X"))))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ V1) == Left(UnknownKeyProblem("VersionedItemId", BPath("Bx") ~ V1)))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ V3) == Left(VersionedItemDeletedProblem(BPath("Bx"))))
    assert(testRepo.idTo[AItem](APath("A") ~ V1) == Right(a1))
    assert(testRepo.idTo[AItem](APath("A") ~ V2) == Right(a2))
    assert(testRepo.idTo[AItem](APath("A") ~ V3) == Right(a3))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ V2) == Right(bx2))
    assert(testRepo.idTo[BItem](BPath("By") ~ V3) == Right(by2))
  }

  "Event output" in {
    assert(testRepo.toEvents.toSeq == TestEvents)
  }

  "eventsFor" in {
    assert(testRepo.eventsFor(Set(APath, BPath)).toSeq == TestEvents)
    assert(testRepo.eventsFor(Set(APath)).toSeq == Seq(
      VersionAdded(V1), VersionedItemAdded(toSigned(a1)),
      VersionAdded(V2), VersionedItemChanged(toSigned(a2)),
      VersionAdded(V3), VersionedItemChanged(toSigned(a3))))
    assert(testRepo.eventsFor(Set(AgentId)).toSeq == Seq(VersionAdded(V1), VersionAdded(V2), VersionAdded(V3)))
  }

  //"pathToCurrentId" in {
  //  assert(testRepo.pathToCurrentId(APath("A")) == Right(APath("A") ~ V3))
  //  assert(testRepo.pathToCurrentId(BPath("A")) == Left(Problem("No such key 'B:/A'")))
  //  assert(testRepo.pathToCurrentId(BPath("Bx")) == Left(Problem("Has been deleted: B:/Bx")))
  //  assert(testRepo.pathToCurrentId(BPath("By")) == Right(BPath("By") ~ V2))
  //  assert(testRepo.pathToCurrentId(APath("X")) == Left(Problem("No such AItem: A:/X")))
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
    assert(testRepo.idTo[AItem](a1.id) == Right(a1))
  }

  "typedCount" in {
    assert(testRepo.typedCount[AItem] == 3)
    assert(testRepo.typedCount[BItem] == 2)
  }

  "currentTyped" in {
    assert(testRepo.currentTyped[AItem] == Map(a3.path -> a3))
    assert(testRepo.currentTyped[BItem] == Map(by2.path -> by2))
  }

  "toEvents" - {
    "VersionedItem with alien version is rejected" in {
      assert(emptyRepo.itemsToEvents(V1, toSigned(a1.withVersion(V2)) :: Nil) == Left(ItemVersionDoesNotMatchProblem(VersionId("1"), a1.path ~ V2)))
    }

    "VersionedItem without version is rejected" in {
      // The signer signs the VersionId, too. It must not be diverge from the commands VersionId
      assert(emptyRepo.itemsToEvents(V1, toSigned(a1.withoutVersion)  :: Nil) == Left(ItemVersionDoesNotMatchProblem(VersionId("1"), a1.path)))
    }

    "VersionedItem with matching version" in {
      assert(emptyRepo.itemsToEvents(V1, toSigned(a1) :: Nil)
        == Right(VersionAdded(V1) :: VersionedItemAdded(toSigned(a1)) :: Nil))
    }

    "Deleting unknown" in {
      assert(emptyRepo.itemsToEvents(V1, Nil, deleted = bx2.path :: Nil)
        == Right(VersionAdded(V1) :: Nil))
    }

    "Duplicate items" in {
      assert(emptyRepo.itemsToEvents(V1, toSigned(a1) :: toSigned(a1) :: Nil)
        == Left(Problem("Unexpected duplicates: 2×A:A")))
    }


    "Duplicate itemsToEvents resulting to same Repo is ignored" - {
      "if some items should be changed" in {
        var repo = emptyRepo
        val events = repo.itemsToEvents(V1, toSigned(a1) :: Nil).orThrow
        assert(events.nonEmpty)
        repo = repo.applyEvents(events).orThrow

        assert(repo.itemsToEvents(V1, toSigned(a1) :: Nil) == Right(Nil))
      }

      "but not if no items should be changed" in {
        var repo = emptyRepo
        var events = repo.itemsToEvents(V1, Nil).orThrow
        assert(events.nonEmpty)
        repo = repo.applyEvents(events).orThrow

        events = repo.itemsToEvents(V1, Nil).orThrow
        assert(events == VersionAdded(V1) :: Nil)
        assert(repo.applyEvents(events) == Left(DuplicateKey("VersionId", V1)))
      }
    }

    "Other" in {
      assert(emptyRepo.itemsToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil, deleted = bx2.path :: Nil)
        == Right(VersionAdded(V1) :: VersionedItemAdded(toSigned(a1)) :: VersionedItemAdded(toSigned(b1)) :: Nil))
    }

    "More" in {
      var repo = emptyRepo
      repo = repo.itemsToEvents(V1, toSigned(a1) :: toSigned(b1) :: Nil).flatMap(repo.applyEvents).orThrow
      assert(repo == Repo.fromOp(V1 :: Nil, Changed(toSigned(a1)) :: Changed(toSigned(b1)) :: Nil, Some(itemVerifier)))

      val events = repo.itemsToEvents(V2, toSigned(a2) :: toSigned(bx2) :: Nil, deleted = b1.path :: Nil).orThrow
      assert(events == VersionAdded(V2) :: VersionedItemDeleted(b1.path) :: VersionedItemChanged(toSigned(a2)) :: VersionedItemAdded(toSigned(bx2)) :: Nil)

      repo = repo.applyEvents(events).orThrow
      assert(repo == Repo.fromOp(V2 :: V1 :: Nil,
        Changed(toSigned(a1)) :: Changed(toSigned(a2)) :: Changed(toSigned(b1)) :: Deleted(b1.path ~ V2) :: Changed(toSigned(bx2)) :: Nil,
        Some(itemVerifier)))

      assert(repo.applyEvents(events) == Left(DuplicateKey("VersionId", VersionId("2"))))
    }
  }

  "applyEvent" - {
    var repo = emptyRepo.applyEvent(VersionAdded(V1)).orThrow

    "VersionedItemChanged for unknown path" in {
      assert(repo.applyEvent(VersionedItemChanged(toSigned(AItem(APath("A") ~ V1, "A")))) ==
        Left(UnknownKeyProblem("ItemPath", APath("A"))))
    }

    "VersionedItemDeleted for unknown path" in {
      assert(repo.applyEvent(VersionedItemDeleted(APath("A"))) ==
        Left(UnknownKeyProblem("ItemPath", APath("A"))))
    }

    "VersionedItemAdded for existent path" in {
      repo = repo.applyEvent(VersionedItemAdded(toSigned(AItem(APath("A") ~ V1, "A")))).orThrow
      assert(repo.applyEvent(VersionedItemAdded(toSigned(AItem(APath("A") ~ V1, "A")))) ==
        Left(DuplicateKey("ItemPath", APath("A"))))
    }

    "FileBaseAdded with different VersionId" in {
      val event = VersionedItemAdded(toSigned(AItem(APath("B") ~ V3, "B")))
      assert(repo.applyEvent(event) ==
        Left(EventVersionDoesNotMatchProblem(V1, event)))
    }

    "FileBaseChanged with different VersionId" in {
      val event = VersionedItemChanged(toSigned(AItem(APath("A") ~ V3, "A")))
      assert(repo.applyEvent(event) == Left(EventVersionDoesNotMatchProblem(V1, event)))
    }

    "VersionedItemChanged for deleted path" in {
      repo = repo.applyEvent(VersionAdded(V2)).orThrow
      repo = repo.applyEvent(VersionedItemDeleted(APath("A"))).orThrow
      repo = repo.applyEvent(VersionAdded(V3)).orThrow
      assert(repo.applyEvent(VersionedItemChanged(toSigned(AItem(APath("A") ~ V3, "A")))) == Left(VersionedItemDeletedProblem(APath("A"))))
    }

    "VersionedItemDeleted for deleted path" in {
      assert(repo.applyEvent(VersionedItemDeleted(APath("A"))) == Left(VersionedItemDeletedProblem(APath("A"))))
    }
  }

  locally {
    val n = sys.props.get("RepoTest").map(_.toInt) getOrElse 2000
    s"Add many $n versions" in {
      // ControllerCommand.UpdateRepo calls itemsToEvents
      var repo = emptyRepo
      val stopwatch = new Stopwatch
      var sw = new Stopwatch
      for (i <- 1 to n) {
        val v = VersionId(i.toString)
        val events = repo.itemsToEvents(v, toSigned(AItem(APath(s"A-$i"), "A") withVersion v) :: Nil).orThrow
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
  //  assert(Repo.diff(a1 :: b1  :: Nil,              Nil) == VersionedItemAdded(a1.withoutVersion) :: VersionedItemAdded(b1.withoutVersion) :: Nil)
  //  assert(Repo.diff(a1 :: b1  :: Nil, a1 ::        Nil) == VersionedItemAdded(b1.withoutVersion) :: Nil)
  //  assert(Repo.diff(             Nil, a1 :: b1  :: Nil) == VersionedItemDeleted(a1.path) :: VersionedItemDeleted(b1.path) :: Nil)
  //  assert(Repo.diff(a1 ::        Nil, a1 :: b1  :: Nil) == VersionedItemDeleted(b1.path) :: Nil)
  //  assert(Repo.diff(a1 :: by2 :: Nil, a1 :: bx2 :: Nil) == VersionedItemDeleted(bx2.path) :: VersionedItemAdded(by2.withoutVersion) :: Nil)
  //}
}

object RepoTest
{
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val a1 = AItem(APath("A") ~ V1, "A-1")
  private val a2 = AItem(APath("A") ~ V2, "A-2")
  private val b1 = BItem(BPath("B") ~ V1, "B-1")
  private val bx2 = BItem(BPath("Bx") ~ V2, "Ba-2")
  private val by2 = BItem(BPath("By") ~ V2, "Bb-2")
  private val a3 = AItem(APath("A") ~ V3, "A-3")

  private implicit val itemJsonCodec = TypedJsonCodec[VersionedItem](
    Subtype[AItem],
    Subtype[BItem])

  private val itemSigner = new VersionedItemSigner(SillySigner.Default, itemJsonCodec)
  private val itemVerifier = new VersionedItemVerifier(new SillySignatureVerifier, itemJsonCodec)
  private val emptyRepo = Repo.signatureVerifying(itemVerifier)

  import itemSigner.toSigned

  private val TestEvents =
    VersionAdded(V1) :: VersionedItemAdded(toSigned(a1)) ::
    VersionAdded(V2) :: VersionedItemChanged(toSigned(a2)) :: VersionedItemAdded(toSigned(bx2)) :: VersionedItemAdded(toSigned(by2)) ::
    VersionAdded(V3) :: VersionedItemChanged(toSigned(a3)) :: VersionedItemDeleted(bx2.path) :: Nil

  private def v(version: String) = VersionId(version)
}
