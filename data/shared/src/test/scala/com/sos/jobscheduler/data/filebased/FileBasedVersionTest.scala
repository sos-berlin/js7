package com.sos.jobscheduler.data.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVersionTest extends FreeSpec
{
  "empty" in {
    assert(FileBasedVersion.checked("") == Invalid(Problem("Empty version identifier?")))
    intercept[ProblemException] {
      FileBasedVersion("")
    }
  }

  "okay" in {
    assert(FileBasedVersion.checked("1") == Valid(FileBasedVersion("1")))
  }
}
