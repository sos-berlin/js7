package js7.tests.addOrders

import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderRemoved, OrderStarted, OrderTerminated}
import js7.data.order.OrderId
import monix.execution.Ack
import monix.reactive.Observer
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.util.Success

private final class StatisticsBuilder(
  isOurOrder: Set[OrderId],
  observer: Observer[Statistics])
{
  private val since = now
  private val orderIdToStarted = mutable.Map[OrderId, Timestamp]()
  private val orderIdToProcessingStarted = mutable.Map[OrderId, Timestamp]()
  private val orderToChildren = mutable.Map[OrderId, Seq[OrderId]]()
  private var eventCount = 0
  private var orderAddedCount = 0
  private var _removedOrderCount = 0
  private var completedForkedOrderCount = 0
  private var totalOrderDuration = 0.s
  private var maximumOrderDuration = 0.s
  private var processedCount = 0
  private var totalProcessDuration = 0.s
  private var maximumProcessDuration = 0.s

  def removedOrderCount = _removedOrderCount
  def lastOrderCount = orderAddedCount - _removedOrderCount

  object obs {
    var observerAck: Future[Ack] = Future.successful(Ack.Continue)
    def tryPublish() =
      if (observerAck.value contains Success(Ack.Continue)) {
        observerAck = observer.onNext(toStatistics)
      }
  }

  obs.tryPublish()  // Initial empty Statistics

  def count(stampedEvent: Stamped[KeyedEvent[Event]]): this.type = {
    import stampedEvent.timestamp

    eventCount += 1
    stampedEvent.value match {
      case KeyedEvent(orderId: OrderId, event) if isOurOrder(orderId.root) =>
        event match {
          case _: OrderAdded =>
            orderAddedCount += 1
            obs.tryPublish()

          case OrderRemoved =>
            _removedOrderCount += 1
            obs.tryPublish()

          case _: OrderStarted =>
            orderIdToStarted(orderId) = timestamp

          case _: OrderTerminated =>
            for (start <- orderIdToStarted.remove(orderId)) {
              val duration = timestamp - start
              totalOrderDuration += duration
              maximumOrderDuration = maximumOrderDuration max duration
            }

          case _: OrderProcessingStarted =>
            orderIdToProcessingStarted(orderId) = timestamp

          case _: OrderProcessed =>
            for (start <- orderIdToProcessingStarted.remove(orderId)) {
              processedCount += 1
              val duration = timestamp - start
              totalProcessDuration += duration
              maximumProcessDuration = maximumProcessDuration max duration
            }

          case OrderForked(children) =>
            orderToChildren(orderId) = children.map(_.orderId)

          case _: OrderJoined =>
            for (children <- orderToChildren.remove(orderId)) {
              completedForkedOrderCount += children.size
            }

          case _ =>
        }
      case _ =>
    }
    this
  }

  def toStatistics = Statistics(
    since.elapsed,
    lastOrderCount = lastOrderCount,
    eventCount = eventCount,
    completedOrderCount = _removedOrderCount,
    totalOrderDuration = totalOrderDuration,
    maximumOrderDuration = maximumOrderDuration,
    completedForkedOrderCount = completedForkedOrderCount,
    processedCount = processedCount,
    totalProcessDuration = totalProcessDuration,
    maximumProcessDuration = maximumProcessDuration)
}
