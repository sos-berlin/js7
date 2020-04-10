package com.sos.jobscheduler.data.filebased

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedTest extends AnyFreeSpec {

  "Companion" in {
    assert(AFileBased.name == "AFileBased")
    assert(AFileBased.toString == "AFileBased")
    assert(AFileBased.typedPathCompanion == APath)
  }
}
