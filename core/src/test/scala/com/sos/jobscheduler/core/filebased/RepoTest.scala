package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.filebased.RepoTest._
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{AFileBased, APath, BFileBased, BPath, VersionId}
import com.sos.jobscheduler.data.job.JobPath
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

    assert(Repo.empty.applyEvent(FileBasedAdded(a1)) ==
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
    assert(testRepo.idTo[BFileBased](BPath("/Ba") % V1) == Invalid(Problem("No such 'B:/Ba 1'")))
    assert(testRepo.idTo[BFileBased](BPath("/Ba") % V3) == Invalid(Problem("Has been deleted: B:/Ba 3")))
    assert(testRepo.idTo[AFileBased](APath("/A") % V1) == Valid(a1))
    assert(testRepo.idTo[AFileBased](APath("/A") % V2) == Valid(a2))
    assert(testRepo.idTo[AFileBased](APath("/A") % V3) == Valid(a3))
    assert(testRepo.idTo[BFileBased](BPath("/Ba") % V2) == Valid(ba2))
    assert(testRepo.idTo[BFileBased](BPath("/Bb") % V3) == Valid(bb2))
  }

  "Event output" in {
    assert(testRepo.toEvents == TestEvents)
  }

  "eventsFor" in {
    assert(testRepo.eventsFor(Set(APath, BPath)) == TestEvents)
    assert(testRepo.eventsFor(Set(APath)) == List(
      VersionAdded(V1), FileBasedAdded(a1),
      VersionAdded(V2), FileBasedChanged(a2),
      VersionAdded(V3), FileBasedChanged(a3)))
    assert(testRepo.eventsFor(Set(JobPath)) == List(VersionAdded(V1), VersionAdded(V2), VersionAdded(V3)))
  }

  "pathToCurrentId" in {
    assert(testRepo.pathToCurrentId(APath("/A")) == Valid(APath("/A") % V3))
    assert(testRepo.pathToCurrentId(BPath("/A")) == Invalid(Problem("No such key 'B:/A'")))
    assert(testRepo.pathToCurrentId(BPath("/Ba")) == Invalid(Problem("Has been deleted: B:/Ba")))
    assert(testRepo.pathToCurrentId(BPath("/Bb")) == Valid(BPath("/Bb") % V2))
    assert(testRepo.pathToCurrentId(APath("/X")) == Invalid(Problem("No such key 'A:/X'")))
  }

  "currentVersion" in {
    assert(testRepo.currentVersion == Map(
      a3.path → a3,
      bb2.path → bb2))
  }

  "versionId" in {
    assert(testRepo.versionId == V3)
    assert(Repo.empty.versionId.isAnonymous)
  }

  "idTo" in {
    assert(testRepo.idTo[AFileBased](a1.id) == Valid(a1))
  }

  "currentTyped" in {
    assert(testRepo.currentTyped[AFileBased] == Map(a3.path → a3))
    assert(testRepo.currentTyped[BFileBased] == Map(bb2.path → bb2))
  }
}

object RepoTest {
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val a1 = AFileBased(APath("/A") % V1, "A-1")
  private val a2 = AFileBased(APath("/A") % V2, "A-2")
  private val ba2 = BFileBased(BPath("/Ba") % V2, "Ba-2")
  private val bb2 = BFileBased(BPath("/Bb") % V2, "Bb-2")
  private val a3 = AFileBased(APath("/A") % V3, "A-3")

  private val TestEvents = List(
    VersionAdded(V1), FileBasedAdded(a1),
    VersionAdded(V2), FileBasedChanged(a2), FileBasedAdded(ba2), FileBasedAdded(bb2),
    VersionAdded(V3), FileBasedChanged(a3), FileBasedDeleted(ba2.path))

  private def v(version: String) = VersionId(version)
}
