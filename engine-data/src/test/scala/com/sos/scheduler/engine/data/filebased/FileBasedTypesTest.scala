package com.sos.scheduler.engine.data.filebased

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedTypesTest extends FreeSpec {

  "forFiles" in {
    import FileBasedType._
    assert(FileBasedTypes.forFiles == Set(Job, JobChain, Lock, Monitor, ProcessClass, Order, Schedule))
  }
}
