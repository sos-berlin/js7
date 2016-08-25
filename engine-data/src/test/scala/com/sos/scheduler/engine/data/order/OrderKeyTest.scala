package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderKeyTest extends FreeSpec {

  "name" in {
    assert((JobChainPath("/a/b") orderKey "1").name == "b,1")
    assert((JobChainPath("/a/b") orderKey "1/2").name == "b,1/2")   // Possible for non-permanent orders
  }
}
