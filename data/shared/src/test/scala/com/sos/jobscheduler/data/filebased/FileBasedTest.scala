package com.sos.jobscheduler.data.filebased

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedTest extends FreeSpec {

  "Companion" in {
    assert(AFileBased.name == "AFileBased")
    assert(AFileBased.toString == "AFileBased")
    assert(AFileBased.typedPathCompanion == APath)
  }
}
