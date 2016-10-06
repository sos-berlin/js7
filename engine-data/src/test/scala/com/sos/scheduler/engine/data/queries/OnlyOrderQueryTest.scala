package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
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
  private val nodeId = NodeId("100")
  private val q = OnlyOrderQuery.Standard()

  "orderIds" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId)
    assert(q.matchesOrder(order))
    assert(q.copy(orderIds = Some(Set(OrderId("1")))) matchesOrder order)
    assert(q.copy(orderIds = Some(Set(OrderId("1"), OrderId("2")))) matchesOrder order)
    assert(!(q.copy(orderIds = Some(Set(OrderId("2")))) matchesOrder order))
  }

  "nodeIds" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId)
    assert(q.matchesOrder(order))
    assert(q.copy(nodeIds = Some(Set(nodeId))) matchesOrder order)
    assert(!q.copy(nodeIds = Some(Set(NodeId("OTHER")))).matchesOrder(order))
  }

  "jobPaths" in {
    val aJobPath = JobPath("/A")
    val bJobPath = JobPath("/B")
    val cJobPath = JobPath("/C")
    val order = QueryableOrder.ForTest(orderKey, nodeId, isSuspended = true)
    assert(q.matchesOrder(order))
    assert(q.copy(jobPaths = Some(Set(aJobPath, bJobPath))).matchesOrder(order, Some(aJobPath)))
    assert(q.copy(jobPaths = Some(Set(aJobPath, bJobPath))).matchesOrder(order, Some(bJobPath)))
    assert(!q.copy(jobPaths = Some(Set(aJobPath, bJobPath))).matchesOrder(order))
    assert(!q.copy(jobPaths = Some(Set(aJobPath, bJobPath))).matchesOrder(order, Some(cJobPath)))
    assert(!q.copy(jobPaths = Some(Set(aJobPath, bJobPath)), isSuspended = Some(false)).matchesOrder(order, Some(aJobPath)))
  }

  "isSuspended" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId, isSuspended = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isSuspended = Some(true)) matchesOrder order)
    assert(!(q.copy(isSuspended = Some(false)) matchesOrder order))
  }

  "isSetback" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId, isSetback = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isSetback = Some(true)) matchesOrder order)
    assert(!(q.copy(isSetback = Some(false)) matchesOrder order))
  }

  "isBlacklisted" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId, isBlacklisted = true)
    assert(q.matchesOrder(order))
    assert(q.copy(isBlacklisted = Some(true)) matchesOrder order)
    assert(!(q.copy(isBlacklisted = Some(false)) matchesOrder order))
  }

  "isOrderProcessingState" in {
    import OrderProcessingState._
    val order = QueryableOrder.ForTest(orderKey, nodeId, orderProcessingStateClass = classOf[Setback])
    assert(q.copy() matchesOrder order)
    assert(!(q.copy(isOrderProcessingState = Some(Set())) matchesOrder order))
    assert(!(q.copy(isOrderProcessingState = Some(Set(NotPlanned.getClass))) matchesOrder order))
    assert(q.copy(isOrderProcessingState = Some(Set(classOf[Setback]))) matchesOrder order)
    assert(q.copy(isOrderProcessingState = Some(Set(NotPlanned.getClass, classOf[Setback]))) matchesOrder order)
  }

  "orIsSuspended" in {
    import OrderProcessingState._
    val setbackOrder = QueryableOrder.ForTest(orderKey, nodeId, orderProcessingStateClass = classOf[Setback])
    val suspendedOrder = QueryableOrder.ForTest(orderKey, nodeId, orderProcessingStateClass = classOf[Planned], isSuspended = true)
    val suspendedSetbackOrder = QueryableOrder.ForTest(orderKey, nodeId, orderProcessingStateClass = classOf[Setback], isSuspended = true)
    val otherOrder = QueryableOrder.ForTest(orderKey, nodeId, orderProcessingStateClass = NotPlanned.getClass)
    val orders = List(setbackOrder, suspendedOrder, suspendedSetbackOrder, otherOrder)

    val orIsSuspendedQuery = q.copy(isOrderProcessingState = Some(Set(classOf[Setback])), orIsSuspended = true)
    assert((orders filter { o ⇒ orIsSuspendedQuery.matchesOrder(o) }) == List(setbackOrder, suspendedOrder, suspendedSetbackOrder))

    val isSuspendedQuery = q.copy(isOrderProcessingState = Some(Set(classOf[Setback])), isSuspended = Some(true))
    assert((orders filter { o ⇒ isSuspendedQuery.matchesOrder(o) }) == List(suspendedSetbackOrder))
  }

  "isOrderSourceType" in {
    val order = QueryableOrder.ForTest(orderKey, nodeId, sourceType = AdHoc)
    assert(q.copy() matchesOrder order)
    assert(!(q.copy(isOrderSourceType = Some(Set())) matchesOrder order))
    assert(!(q.copy(isOrderSourceType = Some(Set(Permanent))) matchesOrder order))
    assert(q.copy(isOrderSourceType = Some(Set(AdHoc, Permanent))) matchesOrder order)
  }
}
