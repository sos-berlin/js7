package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderKey
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TypedPathTest extends FreeSpec {

  "asTyped" in {
    val jobPath = JobPath("/TEST")
    assert(jobPath.asTyped[JobPath] eq jobPath)
    assert(UnknownTypedPath("/TEST").asTyped[JobPath] == jobPath)
    assert(UnknownTypedPath("/TEST,1").asTyped[OrderKey] == (JobChainPath("/TEST") orderKey "1"))
    intercept[IllegalArgumentException] {
      UnknownTypedPath("/TEST,1").asTyped[JobChainPath]
    }
  }
}
