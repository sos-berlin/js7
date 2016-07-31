package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderSourceType._
import com.sos.scheduler.engine.data.order.QueryableOrder
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OnlyOrderQueryTest extends FreeSpec {

  private val orderKey = JobChainPath("/a/jobChain") orderKey "1"
  private val q = OnlyOrderQuery.Standard()

  "isSuspended" in {
    val order = QueryableOrder.ForTest(orderKey, isSuspended = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isSuspended = Some(true)) matchesOrder order)
    assert(!(q.copy(isSuspended = Some(false)) matchesOrder order))
  }

  "isSetback" in {
    val order = QueryableOrder.ForTest(orderKey, isSetback = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isSetback = Some(true)) matchesOrder order)
    assert(!(q.copy(isSetback = Some(false)) matchesOrder order))
  }

  "isBlacklisted" in {
    val order = QueryableOrder.ForTest(orderKey, isBlacklisted = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isBlacklisted = Some(true)) matchesOrder order)
    assert(!(q.copy(isBlacklisted = Some(false)) matchesOrder order))
  }

  "isOrderSourceType" in {
    val order = QueryableOrder.ForTest(orderKey, sourceType = adHoc)
    assert(q.copy() matchesOrder order)
    assert(!(q.copy(isOrderSourceType = Some(Set())) matchesOrder order))
    assert(!(q.copy(isOrderSourceType = Some(Set(fileBased))) matchesOrder order))
    assert(q.copy(isOrderSourceType = Some(Set(adHoc, fileBased))) matchesOrder order)
    assert(q.copy(isOrderSourceType = Some(Set(adHoc, fileBased))) matchesOrder order)
  }
}
