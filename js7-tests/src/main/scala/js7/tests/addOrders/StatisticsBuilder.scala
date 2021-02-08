package js7.tests.addOrders

import java.nio.charset.StandardCharsets.UTF_8
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderRemoved, OrderStarted, OrderStdWritten, OrderTerminated}
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
  private val orderIdToStarted = mutable.Map.empty[OrderId, Timestamp]
  private val orderIdToProcessingStarted = mutable.Map.empty[OrderId, Timestamp]
  private val orderToChildren = mutable.Map.empty[OrderId, Seq[OrderId]]
  private var eventCount = 0
  private var orderAddedCount = 0
  private var _removedOrderCount = 0
  private var failedOrderCount = 0
  private var completedForkedOrderCount = 0
  private var totalOrderDuration = 0.s
  private var maximumOrderDuration = 0.s
  private var processedCount = 0
  private var totalProcessDuration = 0.s
  private var maximumProcessDuration = 0.s
  private var stdWritten = 0L

  def removedOrderCount = _removedOrderCount
  def lastOrderCount = orderAddedCount - _removedOrderCount

  object obs {
    private val pause = 100.ms
    private var next = now
    private var observerAck: Future[Ack] = Future.successful(Ack.Continue)

    def tryPublish() = {
      if (now >= next) {
        if (observerAck.value contains Success(Ack.Continue)) {
          observerAck = observer.onNext(toStatistics)
          next = now + pause
        }
      }
    }
  }

  obs.tryPublish()  // Initial empty Statistics

  def count(stampedEvent: Stamped[KeyedEvent[Event]]): this.type = {
    import stampedEvent.timestamp

    eventCount += 1
    stampedEvent.value match {
      case KeyedEvent(orderId: OrderId, event) if isOurOrder(orderId.root) =>
        event match {
          case event: OrderStdWritten =>
            stdWritten += event.chunk.getBytes(UTF_8).size

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
            obs.tryPublish()

          case _: OrderCancelled | _: OrderFailed | _: OrderFailedInFork =>
            failedOrderCount += 1

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
    failedOrderCount = failedOrderCount,
    totalOrderDuration = totalOrderDuration,
    maximumOrderDuration = maximumOrderDuration,
    completedForkedOrderCount = completedForkedOrderCount,
    processedCount = processedCount,
    totalProcessDuration = totalProcessDuration,
    maximumProcessDuration = maximumProcessDuration,
    stdWritten = stdWritten)
}
