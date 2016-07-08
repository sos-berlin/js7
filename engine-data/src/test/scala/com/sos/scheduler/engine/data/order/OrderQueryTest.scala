package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.jobchain.{JobChainPath, JobChainQuery}
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

  "jobChainQuery" in {
    val order = QueryableOrder.ForTest(orderKey)
    assert(OrderQuery().matches(order))
    assert(OrderQuery(jobChainQuery = JobChainQuery("/")) matches order)
    assert(OrderQuery(jobChainQuery = JobChainQuery("/a/")) matches order)
    assert(!(OrderQuery(jobChainQuery = JobChainQuery("/a")) matches order))
    assert(!(OrderQuery(jobChainQuery = JobChainQuery("/other/")) matches order))
  }

  "isSuspended" in {
    val order = QueryableOrder.ForTest(orderKey, isSuspended = true)
    assert(OrderQuery().matches(order))
    assert(OrderQuery(isSuspended = Some(true)) matches order)
    assert(!(OrderQuery(isSuspended = Some(false)) matches order))
  }

  "isSetback" in {
    val order = QueryableOrder.ForTest(orderKey, isSetback = true)
    assert(OrderQuery().matches(order))
    assert(OrderQuery(isSetback = Some(true)) matches order)
    assert(!(OrderQuery(isSetback = Some(false)) matches order))
  }

  "isBlacklisted" in {
    val order = QueryableOrder.ForTest(orderKey, isBlacklisted = true)
    assert(OrderQuery().matches(order))
    assert(OrderQuery(isBlacklisted = Some(true)) matches order)
    assert(!(OrderQuery(isBlacklisted = Some(false)) matches order))
  }

  "isSourceType" in {
    val order = QueryableOrder.ForTest(orderKey, sourceType = adHoc)
    assert(OrderQuery() matches order)
    assert(!(OrderQuery(isSourceType = Some(Set())) matches order))
    assert(!(OrderQuery(isSourceType = Some(Set(fileBased))) matches order))
    assert(OrderQuery(isSourceType = Some(Set(adHoc, fileBased))) matches order)
    assert(OrderQuery(isSourceType = Some(Set(adHoc, fileBased))) matches order)
  }
}
