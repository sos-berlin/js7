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
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemVersionDoesNotMatchProblem, VersionedItemRemovedProblem}
import js7.data.item.Repo.testOnly.{Changed, OpRepo, Removed}
import js7.data.item.RepoTest._
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged, VersionedItemRemoved}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends AnyFreeSpec
{
  import itemSigner.sign

  private lazy val Right(testRepo: Repo) = emptyRepo.applyEvents(versionedEvents)

  "empty" in {
    assert(emptyRepo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(emptyRepo.idTo[AItem](APath("UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("VersionedItemPath", APath("UNKNOWN-PATH"))))

    assert(emptyRepo.applyEvent(VersionedItemAdded(Signed(a1, SignedString(a1.asJson.compactPrint, GenericSignature("SILLY", "SIGNED"))))) ==
      Left(Problem("Missing initial VersionAdded event for Repo")))

    assert(!emptyRepo.exists(APath("A")))
  }

  "empty version" in {
    val Right(repo) = emptyRepo.applyEvent(VersionAdded(v("INITIAL")))
    assert(repo.historyBefore(v("INITIAL")) == Right(Nil))
    assert(repo.historyBefore(v("UNKNOWN")) == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))

    assert(repo.idTo[AItem](APath("UNKNOWN") ~ "INITIAL") ==
      Left(UnknownKeyProblem("VersionedItemPath", APath("UNKNOWN"))))

    assert(repo.idTo[AItem](APath("UNKNOWN-PATH") ~ "UNKNOWN-VERSION") ==
      Left(UnknownKeyProblem("VersionedItemPath", APath("UNKNOWN-PATH"))))

    assert(repo.applyEvent(VersionAdded(v("INITIAL"))) ==
      Left(DuplicateKey("VersionId", v("INITIAL"))))
  }

  "exists" in {
    assert(testRepo.exists(APath("A")))
    assert(!testRepo.exists(APath("Bx")))
    assert(!testRepo.exists(APath("UNKNOWN")))
  }

  "markedAsRemoved" in {
    assert(!testRepo.markedAsRemoved(APath("A")))
    assert(!testRepo.markedAsRemoved(APath("UNKNOWN")))
    assert(!testRepo.markedAsRemoved(BPath("B")))
    assert(testRepo.markedAsRemoved(BPath("Bx")))
    assert(!testRepo.markedAsRemoved(BPath("By")))
  }

  "Event input" in {
    assert(testRepo.idTo[AItem](APath("A") ~ "UNKNOWN") == Left(UnknownKeyProblem("VersionId", VersionId("UNKNOWN"))))
    assert(testRepo.idTo[AItem](APath("X") ~ v1) == Left(UnknownKeyProblem("VersionedItemPath", APath("X"))))
    assert(testRepo.idTo[AItem](APath("X") ~ v1) == Left(UnknownKeyProblem("VersionedItemPath", APath("X"))))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ v1) == Left(UnknownKeyProblem("VersionedItemId", BPath("Bx") ~ v1)))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ v3) == Left(VersionedItemRemovedProblem(BPath("Bx"))))
    assert(testRepo.idTo[AItem](APath("A") ~ v1) == Right(a1))
    assert(testRepo.idTo[AItem](APath("A") ~ v2) == Right(a2))
    assert(testRepo.idTo[AItem](APath("A") ~ v3) == Right(a3))
    assert(testRepo.idTo[BItem](BPath("Bx") ~ v2) == Right(bx2))
    assert(testRepo.idTo[BItem](BPath("By") ~ v3) == Right(by2))
  }

  "Event output" in {
    assert(testRepo.toEvents.toSeq == snapshotEvents)
  }

  //"pathToCurrentId" in {
  //  assert(testRepo.pathToCurrentId(APath("A")) == Right(APath("A") ~ v3))
  //  assert(testRepo.pathToCurrentId(BPath("A")) == Left(Problem("No such key 'B:/A'")))
  //  assert(testRepo.pathToCurrentId(BPath("Bx")) == Left(Problem("Has been deleted: B:/Bx")))
  //  assert(testRepo.pathToCurrentId(BPath("By")) == Right(BPath("By") ~ v2))
  //  assert(testRepo.pathToCurrentId(APath("X")) == Left(Problem("No such AItem: A:/X")))
  //}

  "currentVersion" in {
    assert(testRepo.currentVersion == Map(
      a3.path -> sign(a3),
      by2.path -> sign(by2)))
  }

  "versionId" in {
    assert(testRepo.versionId == v3)
    assert(emptyRepo.versionId.isAnonymous)
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
      assert(emptyRepo.itemsToEventBlock(v1, sign(a1.withVersion(v2)) :: Nil) == Left(ItemVersionDoesNotMatchProblem(VersionId("1"), a1.path ~ v2)))
    }

    "VersionedItem without version is rejected" in {
      // The signer signs the VersionId, too. It must not be diverge from the commands VersionId
      assert(emptyRepo.itemsToEventBlock(v1, sign(a1.withoutVersion)  :: Nil) == Left(ItemVersionDoesNotMatchProblem(VersionId("1"), a1.path)))
    }

    "VersionedItem with matching version" in {
      assert(emptyRepo.itemsToEventBlock(v1, sign(a1) :: Nil)
        == Right(emptyRepo.NonEmptyEventBlock(v1, Nil, VersionedItemAdded(sign(a1)) :: Nil)))
    }

    "Deleting unknown" in {
      assert(emptyRepo.itemsToEventBlock(v1, Nil, removed = bx2.path :: Nil)
        == Right(emptyRepo.NonEmptyEventBlock(v1, Nil, Nil)))
    }

    "Duplicate items" in {
      assert(emptyRepo.itemsToEventBlock(v1, sign(a1) :: sign(a1) :: Nil)
        == Left(Problem("Unexpected duplicates: 2×A:A")))
    }

    "Duplicate itemsToEventBlock resulting to same Repo is ignored" - {
      "if some items should be changed" in {
        var repo = emptyRepo
        val eventBlock = repo.itemsToEventBlock(v1, sign(a1) :: Nil).orThrow
        assert(eventBlock.nonEmpty)
        repo = repo.applyEvents(eventBlock.events).orThrow

        assert(repo.itemsToEventBlock(v1, sign(a1) :: Nil) == Right(repo.emptyEventBlock))
      }

      "but not if no items should be changed" in {
        var repo = emptyRepo
        var eventBlock = repo.itemsToEventBlock(v1, Nil).orThrow
        assert(eventBlock.nonEmpty)
        repo = repo.applyEvents(eventBlock.events).orThrow

        eventBlock = repo.itemsToEventBlock(v1, Nil).orThrow
        assert(eventBlock == repo.NonEmptyEventBlock(v1, Nil, Nil))
        assert(repo.applyEvents(eventBlock.events) == Left(DuplicateKey("VersionId", v1)))
      }
    }

    "Other" in {
      assert(emptyRepo.itemsToEventBlock(v1, sign(a1) :: sign(b1) :: Nil, removed = bx2.path :: Nil)
        == Right(emptyRepo.NonEmptyEventBlock(v1, Nil, VersionedItemAdded(sign(a1)) :: VersionedItemAdded(sign(b1)) :: Nil)))
    }

    "More" in {
      var repo = emptyRepo
      repo = repo.itemsToEventBlock(v1, sign(a1) :: sign(b1) :: Nil).map(_.events).flatMap(repo.applyEvents).orThrow
      assert(repo == Repo.fromOp(v1 :: Nil, Changed(sign(a1)) :: Changed(sign(b1)) :: Nil, Some(signatureVerifier)))

      val eventBlock = repo.itemsToEventBlock(v2, sign(a2) :: sign(bx2) :: Nil, removed = b1.path :: Nil).orThrow
      assert(eventBlock == repo.NonEmptyEventBlock(
        v2,
        removedEvents = Seq(VersionedItemRemoved(b1.path)),
        addedOrChanged = Seq(VersionedItemChanged(sign(a2)), VersionedItemAdded(sign(bx2)))))

      repo = repo.applyEvents(eventBlock.events).orThrow
      assert(repo == Repo.fromOp(
        Seq(v2, v1),
        Seq(
          Changed(sign(a1)),
          Changed(sign(a2)),
          Changed(sign(b1)),
          Removed(b1.path ~ v2),
          Changed(sign(bx2))),
        Some(signatureVerifier)))

      assert(repo.applyEvents(eventBlock.events) == Left(DuplicateKey("VersionId", v2)))

      assert(repo.toEvents.toSeq == Seq(
        VersionAdded(v1),
        VersionedItemAdded(sign(a1)),
        VersionedItemAdded(sign(b1)),
        VersionAdded(v2),
        VersionedItemChanged(sign(a2)),
        VersionedItemRemoved(b1.path),
        VersionedItemAdded(sign(bx2))))
    }
  }

  "deleteItem" - {
    "Delete the only Item in the Repo" in {
      val repo = emptyRepo
        .applyEvents(Seq(
          VersionAdded(v1), VersionedItemAdded(sign(a1)),
          VersionAdded(v2), VersionedItemRemoved(a1.path)))
        .orThrow
      assert(emptyRepo.applyEvents(repo.toEvents) == Right(repo))

      val deleted1 = repo.deleteItem(a1.id).orThrow
      assert(deleted1 == emptyRepo)
      assert(deleted1.toEvents.isEmpty)
      assert(emptyRepo.applyEvents(deleted1.toEvents) == Right(deleted1))
    }

    "Delete the only Item (but some other Paths exist)" in {
      val repo = emptyRepo
        .applyEvents(Seq(
          VersionAdded(v1), VersionedItemAdded(sign(a1)), VersionedItemAdded(sign(b1)),
          VersionAdded(v2), VersionedItemRemoved(a1.path)))
        .orThrow
      assert(emptyRepo.applyEvents(repo.toEvents) == Right(repo))

      val b1Deleted = repo.deleteItem(b1.id).orThrow
      assert(b1Deleted.toEvents.toSeq == Seq(
        VersionAdded(v1), VersionedItemAdded(sign(a1)),
        VersionAdded(v2), VersionedItemRemoved(a1.path)))
      assert(emptyRepo.applyEvents(b1Deleted.toEvents) == Right(b1Deleted))

      assert(b1Deleted.deleteItem(a1.id).orThrow == emptyRepo)
    }

    "Delete an old removed item" in {
      val repo = emptyRepo
        .applyEvents(Seq(
          VersionAdded(v1), VersionedItemAdded(sign(a1)),
          VersionAdded(v2), VersionedItemRemoved(a1.path),
          VersionAdded(v3), VersionedItemAdded(sign(a3))))
        .orThrow

      assert(emptyRepo.applyEvents(repo.toEvents) == Right(repo))

      val a1Deleted = repo.deleteItem(a1.id).orThrow
      assert(a1Deleted.toEvents.toSeq == Seq(
        VersionAdded(v3), VersionedItemAdded(sign(a3))))
      assert(emptyRepo.applyEvents(a1Deleted.toEvents) == Right(a1Deleted))

      val a2Deleted = a1Deleted.deleteItem(a2.id).orThrow
      assert(a2Deleted.toEvents.toSeq == Seq(
        VersionAdded(v3), VersionedItemAdded(sign(a3))))
      assert(emptyRepo.applyEvents(a2Deleted.toEvents) == Right(a2Deleted))
    }
  }

  "unusedItemIdsForType" in {
    assert(emptyRepo.unusedItemIdsForType[APath](Set.empty).isEmpty)
    assert(testRepo.unusedItemIdsForType[APath](Set.empty).toSeq == Seq(a2.id, a1.id))
    assert(testRepo.unusedItemIdsForType(Set(a1.id)).toSeq == Seq(a2.id))
    assert(testRepo.unusedItemIdsForType(Set(a2.id)).toSeq == Seq(a1.id))
    assert(testRepo.unusedItemIdsForType(Set(a3.id)).toSeq == Seq(a2.id, a1.id))
    assert(testRepo.unusedItemIdsForType(Set(a1.id, a2.id, a3.id)).toSeq == Seq())

    assert(testRepo.unusedItemIdsForType[BPath](Set.empty).toSet == Set(bx3.id, bx2.id))

    // bx3 cannot be in use because it's deleted. Anyway, unusedItemIdsForType should work properly
    assert(testRepo.unusedItemIdsForType(Set(bx3.id)).toSeq == Seq(bx2.id, bx3.id))
    assert(testRepo.unusedItemIdsForType(Set(bx3.id, by2.id)).toSeq == Seq(bx2.id, bx3.id))

    assert(testRepo.unusedItemIdsForType[BPath](Set.empty).toSeq == Seq(bx2.id, bx3.id))
    assert(testRepo.unusedItemIdsForType(Set(bx2.id)).isEmpty)
    assert(testRepo.unusedItemIdsForType(Set(by2.id)).toSeq == Seq(bx2.id, bx3.id))
  }

  "applyEvent" - {
    var repo = emptyRepo.applyEvent(VersionAdded(v1)).orThrow

    "VersionedItemChanged for unknown path" in {
      assert(repo.applyEvent(VersionedItemChanged(sign(AItem(APath("A") ~ v1, "A")))) ==
        Left(UnknownKeyProblem("VersionedItemPath", APath("A"))))
    }

    "VersionedItemRemoved for unknown path" in {
      assert(repo.applyEvent(VersionedItemRemoved(APath("A"))) ==
        Left(UnknownKeyProblem("VersionedItemPath", APath("A"))))
    }

    "VersionedItemAdded for existent path" in {
      repo = repo.applyEvent(VersionedItemAdded(sign(AItem(APath("A") ~ v1, "A")))).orThrow
      assert(repo.applyEvent(VersionedItemAdded(sign(AItem(APath("A") ~ v1, "A")))) ==
        Left(DuplicateKey("VersionedItemPath", APath("A"))))
    }

    "FileBaseAdded with different VersionId" in {
      val event = VersionedItemAdded(sign(AItem(APath("B") ~ v3, "B")))
      assert(repo.applyEvent(event) ==
        Left(EventVersionDoesNotMatchProblem(v1, event)))
    }

    "FileBaseChanged with different VersionId" in {
      val event = VersionedItemChanged(sign(AItem(APath("A") ~ v3, "A")))
      assert(repo.applyEvent(event) == Left(EventVersionDoesNotMatchProblem(v1, event)))
    }

    "VersionedItemChanged for removed path" in {
      repo = repo.applyEvent(VersionAdded(v2)).orThrow
      repo = repo.applyEvent(VersionedItemRemoved(APath("A"))).orThrow
      repo = repo.applyEvent(VersionAdded(v3)).orThrow
      assert(repo.applyEvent(VersionedItemChanged(sign(AItem(APath("A") ~ v3, "A")))) == Left(VersionedItemRemovedProblem(APath("A"))))
    }

    "VersionedItemRemoved for removed path" in {
      assert(repo.applyEvent(VersionedItemRemoved(APath("A"))) == Left(VersionedItemRemovedProblem(APath("A"))))
    }
  }

  locally {
    val n = sys.props.get("RepoTest").map(_.toInt) getOrElse 100
    s"Add many ($n) versions" in {
      // ControllerCommand.UpdateRepo calls itemsToEventBlock
      var repo = emptyRepo
      val stopwatch = new Stopwatch
      var sw = new Stopwatch
      for (i <- 1 to n) {
        val v = VersionId(i.toString)
        val eventBlock = repo.itemsToEventBlock(v, sign(AItem(APath(s"A-$i"), "A") withVersion v) :: Nil).orThrow
        repo = repo.applyEvents(eventBlock.events).orThrow
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
  //  assert(Repo.diff(             Nil, a1 :: b1  :: Nil) == VersionedItemRemoved(a1.path) :: VersionedItemRemoved(b1.path) :: Nil)
  //  assert(Repo.diff(a1 ::        Nil, a1 :: b1  :: Nil) == VersionedItemRemoved(b1.path) :: Nil)
  //  assert(Repo.diff(a1 :: by2 :: Nil, a1 :: bx2 :: Nil) == VersionedItemRemoved(bx2.path) :: VersionedItemAdded(by2.withoutVersion) :: Nil)
  //}
}

object RepoTest
{
  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val v3 = VersionId("3")
  private val a1 = AItem(APath("A") ~ v1, "A-1")
  private val a2 = AItem(APath("A") ~ v2, "A-2")
  private val b1 = BItem(BPath("B") ~ v1, "B-1")
  private val bx2 = BItem(BPath("Bx") ~ v2, "Bx-2")
  private val bx3 = BItem(BPath("Bx") ~ v3, "Bx-3")
  private val by2 = BItem(BPath("By") ~ v2, "By-2")
  private val a3 = AItem(APath("A") ~ v3, "A-3")

  private implicit val itemJsonCodec = TypedJsonCodec[VersionedItem](
    Subtype[AItem],
    Subtype[BItem])

  private val itemSigner = new ItemSigner(SillySigner.Default, itemJsonCodec)
  private val signatureVerifier = SillySignatureVerifier.Default
  private val emptyRepo = Repo.signatureVerifying(signatureVerifier).copy(selfTest = true)

  import itemSigner.sign

  private val versionedEvents = Seq(
    VersionAdded(v1), VersionedItemAdded(sign(a1)),
    VersionAdded(v2), VersionedItemChanged(sign(a2)), VersionedItemAdded(sign(bx2)), VersionedItemAdded(sign(by2)),
    VersionAdded(v3), VersionedItemChanged(sign(a3)), VersionedItemRemoved(bx3.path))

  private val snapshotEvents = versionedEvents

  private def v(version: String) = VersionId(version)
}
