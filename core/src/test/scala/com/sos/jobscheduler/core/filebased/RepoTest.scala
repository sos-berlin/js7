package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.filebased.RepoTest._
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBasedVersion, TestFileBased, TestPath, TypedPath}
import com.sos.jobscheduler.data.workflow.JobPath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoTest extends FreeSpec
{
  private lazy val Valid(testRepo) = Repo.empty.applyEvents(TestEvents)

  "empty" in {
    assert(Repo.empty.historyBefore(v("UNKNOWN")) == Invalid(Problem("No such 'version UNKNOWN'")))

    assert(Repo.empty.get(TypedPath.Versioned(v("UNKNOWN-VERSION"), TestPath("/UNKNOWN-PATH"))) ==
      Invalid(Problem("No such key 'Test:/UNKNOWN-PATH'")))

    assert(Repo.empty.applyEvent(FileBasedAdded(a1)) ==
      Invalid(Problem("Missing first event VersionAdded for Repo")))
  }

  "empty version" in {
    val Valid(repo) = Repo.empty.applyEvent(VersionAdded(v("INITIAL")))
    assert(repo.historyBefore(v("INITIAL")) == Valid(Nil))
    assert(repo.historyBefore(v("UNKNOWN")) == Invalid(Problem("No such 'version UNKNOWN'")))

    assert(repo.get(TypedPath.Versioned(v("INITIAL"), TestPath("/UNKNOWN"))) ==
      Invalid(Problem("No such key 'Test:/UNKNOWN'")))

    assert(repo.get(TypedPath.Versioned(v("UNKNOWN-VERSION"), TestPath("/UNKNOWN-PATH"))) ==
      Invalid(Problem("No such key 'Test:/UNKNOWN-PATH'")))

    assert(repo.applyEvent(VersionAdded(v("INITIAL"))) ==
      Invalid(Repo.DuplicateVersionProblem(v("INITIAL"))))
  }

  "Event input" in {
    assert(testRepo.get(TestPath.Versioned(v("1"), TestPath("/A"))) == Valid(a1))
    assert(testRepo.get(TestPath.Versioned(v("2"), TestPath("/A"))) == Valid(a2))
    assert(testRepo.get(TestPath.Versioned(v("3"), TestPath("/A"))) == Valid(a3))
    assert(testRepo.get(TestPath.Versioned(v("4"), TestPath("/A"))) == Invalid(Problem("No such 'version 4'")))
    assert(testRepo.get(TestPath.Versioned(v("1"), TestPath("/B"))) == Invalid(Problem("Not found: Versioned(version 1,Test:/B)")))
    assert(testRepo.get(TestPath.Versioned(v("2"), TestPath("/B"))) == Valid(b2))
    assert(testRepo.get(TestPath.Versioned(v("3"), TestPath("/B"))) == Invalid(Problem("Has been deleted: Versioned(version 3,Test:/B)")))
    assert(testRepo.get(TestPath.Versioned(v("3"), TestPath("/C"))) == Valid(c2))
  }

  "Event output" in {
    assert(testRepo.toEvents == TestEvents)
  }

  "eventsFor" in {
    assert(testRepo.eventsFor(Set(TestPath)) == TestEvents)
    assert(testRepo.eventsFor(Set(JobPath)) == List(VersionAdded(v("1")), VersionAdded(v("2")), VersionAdded(v("3"))))
  }

  "currentVersion" in {
    assert(testRepo.currentVersion == Map(
      TestPath("/A") → a3,
      TestPath("/C") → c2))
  }
}

object RepoTest {
  private val a1 = TestFileBased(TestPath("/A"), "A-1")
  private val a2 = TestFileBased(TestPath("/A"), "A-2")
  private val b2 = TestFileBased(TestPath("/B"), "B-2")
  private val c2 = TestFileBased(TestPath("/C"), "C-2")
  private val a3 = TestFileBased(TestPath("/A"), "A-3")

  private val TestEvents = List(
    VersionAdded(v("1")),
    FileBasedAdded(a1),
    VersionAdded(v("2")),
    FileBasedChanged(a2),
    FileBasedAdded(b2),
    FileBasedAdded(c2),
    VersionAdded(v("3")),
    FileBasedChanged(a3),
    FileBasedDeleted(TestPath("/B")))

  private def v(version: String) = FileBasedVersion(version)
}
