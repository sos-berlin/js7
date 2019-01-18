package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.filebased.Repo.{Changed, Deleted}
import com.sos.jobscheduler.core.filebased.RepoTest._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{AFileBased, APath, BFileBased, BPath, VersionId}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends FreeSpec
{
  private lazy val Valid(testRepo) = Repo.empty.applyEvents(TestEvents)

  "empty" in {
    assert(Repo.empty.historyBefore(v("UNKNOWN")) == Invalid(Problem("No such 'version UNKNOWN'")))

    assert(Repo.empty.idTo[AFileBased](APath("/UNKNOWN-PATH") % "UNKNOWN-VERSION") ==
      Invalid(Problem("No such key 'A:/UNKNOWN-PATH'")))

    assert(Repo.empty.applyEvent(FileBasedAdded(a1.withoutVersion)) ==
      Invalid(Problem("Missing first event VersionAdded for Repo")))
  }

  "empty version" in {
    val Valid(repo) = Repo.empty.applyEvent(VersionAdded(v("INITIAL")))
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
      VersionAdded(V1), FileBasedAdded(a1.withoutVersion),
      VersionAdded(V2), FileBasedChanged(a2.withoutVersion),
      VersionAdded(V3), FileBasedChanged(a3.withoutVersion)))
    assert(testRepo.eventsFor(Set(AgentPath)) == List(VersionAdded(V1), VersionAdded(V2), VersionAdded(V3)))
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
      a3.path → a3,
      by2.path → by2))
  }

  "versionId" in {
    assert(testRepo.versionId == V3)
    assert(Repo.empty.versionId.isAnonymous)
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

  "currentTyped" in {
    assert(testRepo.currentTyped[AFileBased] == Map(a3.path → a3))
    assert(testRepo.currentTyped[BFileBased] == Map(by2.path → by2))
  }

  "toEvent" - {
    "FileBased with alien version is rejected" in {
      assert(Repo.empty.fileBasedToEvents(V1, a1.withVersion(V2) :: Nil) == Invalid(Problem("Expected version '1' in 'A:/A 2'")))
    }

    "FileBased without version" in {
      assert(Repo.empty.fileBasedToEvents(V1, a1.withoutVersion  :: Nil)
        == Valid(VersionAdded(V1) :: FileBasedAdded(a1.withoutVersion) :: Nil))
    }

    "FileBased with matching version" in {
      assert(Repo.empty.fileBasedToEvents(V1, a1 :: Nil)
        == Valid(VersionAdded(V1) :: FileBasedAdded(a1.withoutVersion) :: Nil))
    }

    "Deleting unknown" in {
      assert(Repo.empty.fileBasedToEvents(V1, Nil, deleted = bx2.path :: Nil)
        == Valid(VersionAdded(V1) :: Nil))
    }

    "Duplicate" in {
      assert(Repo.empty.fileBasedToEvents(V1, a1 :: a1 :: Nil)
        == Invalid(Problem("Unexpected duplicates: A:/A")))
    }

    "Other" in {
      assert(Repo.empty.fileBasedToEvents(V1, a1 :: b1 :: Nil, deleted = bx2.path :: Nil)
        == Valid(VersionAdded(V1) :: FileBasedAdded(a1.withoutVersion) :: FileBasedAdded(b1.withoutVersion) :: Nil))
    }

    "More" in {
      var repo = Repo.empty
      repo = repo.fileBasedToEvents(V1, a1 :: b1 :: Nil).flatMap(repo.applyEvents).orThrow
      assert(repo == Repo(V1 :: Nil, Changed(a1) :: Changed(b1) :: Nil))

      val events = repo.fileBasedToEvents(V2, a2 :: bx2 :: Nil, deleted = b1.path :: Nil).orThrow
      assert(events == VersionAdded(V2) :: FileBasedDeleted(b1.path) :: FileBasedChanged(a2.withoutVersion) :: FileBasedAdded(bx2.withoutVersion) :: Nil)

      repo = repo.applyEvents(events).orThrow
      assert(repo == Repo(V2 :: V1 :: Nil, Changed(a1) :: Changed(a2) :: Changed(b1) :: Deleted(b1.path % V2) :: Changed(bx2) :: Nil))

      assert(repo.applyEvents(events) == Invalid(Problem("Duplicate VersionId '2'")))
    }
  }

  "diff" in {
    // NOT USED ?
    assert(Repo.diff(a1 :: b1  :: Nil, a1 :: b1  :: Nil) == Nil)
    assert(Repo.diff(a1 :: b1  :: Nil,              Nil) == FileBasedAdded(a1.withoutVersion) :: FileBasedAdded(b1.withoutVersion) :: Nil)
    assert(Repo.diff(a1 :: b1  :: Nil, a1 ::        Nil) == FileBasedAdded(b1.withoutVersion) :: Nil)
    assert(Repo.diff(             Nil, a1 :: b1  :: Nil) == FileBasedDeleted(a1.path) :: FileBasedDeleted(b1.path) :: Nil)
    assert(Repo.diff(a1 ::        Nil, a1 :: b1  :: Nil) == FileBasedDeleted(b1.path) :: Nil)
    assert(Repo.diff(a1 :: by2 :: Nil, a1 :: bx2 :: Nil) == FileBasedDeleted(bx2.path) :: FileBasedAdded(by2.withoutVersion) :: Nil)
  }
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

  private val TestEvents =
    VersionAdded(V1) :: FileBasedAdded(a1.withoutVersion) ::
    VersionAdded(V2) :: FileBasedChanged(a2.withoutVersion) :: FileBasedAdded(bx2.withoutVersion) :: FileBasedAdded(by2.withoutVersion) ::
    VersionAdded(V3) :: FileBasedChanged(a3.withoutVersion) :: FileBasedDeleted(bx2.path) :: Nil

  private def v(version: String) = VersionId(version)
}
