package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderSourceType._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderQueryTest extends FreeSpec {

  private val orderKey = JobChainPath("/a/jobChain") orderKey "1"

  "jobChains" in {
    val order = QueryableOrder.ForTest(orderKey)
    assert(OrderQuery()(order))
    assert(OrderQuery(jobChains = "/")(order))
    assert(OrderQuery(jobChains = "/a/")(order))
    assert(!OrderQuery(jobChains = "/a")(order))
    assert(!OrderQuery(jobChains = "/other/")(order))
  }

  "isSuspended" in {
    val order = QueryableOrder.ForTest(orderKey, isSuspended = true)
    assert(OrderQuery()(order))
    assert(OrderQuery(isSuspended = Some(true))(order))
    assert(!OrderQuery(isSuspended = Some(false))(order))
  }

  "isSetback" in {
    val order = QueryableOrder.ForTest(orderKey, isSetback = true)
    assert(OrderQuery()(order))
    assert(OrderQuery(isSetback = Some(true))(order))
    assert(!OrderQuery(isSetback = Some(false))(order))
  }

  "isBlacklisted" in {
    val order = QueryableOrder.ForTest(orderKey, isBlacklisted = true)
    assert(OrderQuery()(order))
    assert(OrderQuery(isBlacklisted = Some(true))(order))
    assert(!OrderQuery(isBlacklisted = Some(false))(order))
  }

  "isSourceType" in {
    val order = QueryableOrder.ForTest(orderKey, sourceType = adHoc)
    assert(OrderQuery()(order))
    assert(!OrderQuery(isSourceType = Some(Set()))(order))
    assert(!OrderQuery(isSourceType = Some(Set(fileBased)))(order))
    assert(OrderQuery(isSourceType = Some(Set(adHoc, fileBased)))(order))
    assert(OrderQuery(isSourceType = Some(Set(adHoc, fileBased)))(order))
  }
}
