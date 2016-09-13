package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderSourceType._
import com.sos.scheduler.engine.data.order.{OrderId, OrderProcessingState}
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

  "orderId" in {
    val order = QueryableOrder.ForTest(orderKey)
    assert(q.matchesOrder(order))
    assert(q.copy(orderId = Some(OrderId("1"))) matchesOrder order)
    assert(!(q.copy(orderId = Some(OrderId("2"))) matchesOrder order))
  }

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

  "isOrderProcessingState" in {
    import OrderProcessingState._
    val order = QueryableOrder.ForTest(orderKey, processingStateClass = classOf[Setback])
    assert(q.copy() matchesOrder order)
    assert(!(q.copy(isOrderProcessingState = Some(Set())) matchesOrder order))
    assert(!(q.copy(isOrderProcessingState = Some(Set(NotPlanned.getClass))) matchesOrder order))
    assert(q.copy(isOrderProcessingState = Some(Set(classOf[Setback]))) matchesOrder order)
    assert(q.copy(isOrderProcessingState = Some(Set(NotPlanned.getClass, classOf[Setback]))) matchesOrder order)
  }

  "orIsSuspended" in {
    import OrderProcessingState._
    val setbackOrder = QueryableOrder.ForTest(orderKey, processingStateClass = classOf[Setback])
    val suspendedOrder = QueryableOrder.ForTest(orderKey, processingStateClass = classOf[Planned], isSuspended = true)
    val suspendedSetbackOrder = QueryableOrder.ForTest(orderKey, processingStateClass = classOf[Setback], isSuspended = true)
    val otherOrder = QueryableOrder.ForTest(orderKey, processingStateClass = NotPlanned.getClass)
    val orders = List(setbackOrder, suspendedOrder, suspendedSetbackOrder, otherOrder)

    val orIsSuspendedQuery = q.copy(isOrderProcessingState = Some(Set(classOf[Setback])), orIsSuspended = true)
    assert((orders filter orIsSuspendedQuery.matchesOrder) == List(setbackOrder, suspendedOrder, suspendedSetbackOrder))

    val isSuspendedQuery = q.copy(isOrderProcessingState = Some(Set(classOf[Setback])), isSuspended = Some(true))
    assert((orders filter isSuspendedQuery.matchesOrder) == List(suspendedSetbackOrder))
  }

  "isOrderSourceType" in {
    val order = QueryableOrder.ForTest(orderKey, sourceType = AdHoc)
    assert(q.copy() matchesOrder order)
    assert(!(q.copy(isOrderSourceType = Some(Set())) matchesOrder order))
    assert(!(q.copy(isOrderSourceType = Some(Set(Permanent))) matchesOrder order))
    assert(q.copy(isOrderSourceType = Some(Set(AdHoc, Permanent))) matchesOrder order)
  }
}
