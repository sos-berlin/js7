package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.FileBasedType._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedTypesTest extends FreeSpec {

  "forFiles" in {
    assert(FileBasedTypes.forFiles == Set(job, jobChain, lock, monitor, processClass, order, schedule))
  }
}
