package com.sos.scheduler.engine.data.filebased

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TypedPathTest extends FreeSpec {

  "filenameExtensions" in {
    assert(TypedPath.extensions ==
      Set(".job.xml", ".job_chain.xml", ".lock.xml", ".monitor.xml", ".process_class.xml", ".order.xml", ".schedule.xml"))
  }
}
