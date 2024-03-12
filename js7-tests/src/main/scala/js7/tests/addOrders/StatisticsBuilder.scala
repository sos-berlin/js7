package js7.tests.addOrders

import cats.effect.IO
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAddedX, OrderCancelled, OrderDeleted, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTerminated}
import js7.data.order.OrderId
import scala.collection.mutable
import scala.concurrent.duration.Deadline

private final class StatisticsBuilder(
  isOurOrder: Set[OrderId],
  tryEmit: Statistics => IO[Boolean]):

  private val since = Deadline.now
  private val orderIdToStarted = mutable.Map.empty[OrderId, Timestamp]
  private val orderIdToProcessingStarted = mutable.Map.empty[OrderId, Timestamp]
  private val orderToChildren = mutable.Map.empty[OrderId, Seq[OrderId]]
  private var eventCount = 0
  private var orderAddedCount = 0
  private var _deletedOrderCount = 0
  private var failedOrderCount = 0
  private var completedForkedOrderCount = 0
  private var totalOrderDuration = 0.s
  private var maximumOrderDuration = 0.s
  private var processedCount = 0
  private var totalProcessDuration = 0.s
  private var maximumProcessDuration = 0.s
  private var stdWritten = 0L

  def deletedOrderCount = _deletedOrderCount
  def lastOrderCount = orderAddedCount - _deletedOrderCount

  object obs:
    private val pause = 100.ms
    private var next = Deadline.now

    def tryPublish(): IO[Unit] =
      IO.defer:
        val now = Deadline.now
        IO.whenA(now >= next):
          tryEmit(toStatistics)
            .map: _ =>
              next = now + pause

  def count(stampedEvent: Stamped[KeyedEvent[Event]]): IO[Unit] =
    import stampedEvent.timestamp

    IO.defer:
      eventCount += 1
      stampedEvent.value match
        case KeyedEvent(orderId: OrderId, event) if isOurOrder(orderId.root) =>
          event match
            case event: OrderStdWritten =>
              stdWritten += event.chunk.getBytes(UTF_8).size
              IO.unit

            case _: OrderAddedX =>
              orderAddedCount += 1
              obs.tryPublish()

            case OrderDeleted =>
              _deletedOrderCount += 1
              obs.tryPublish()

            case _: OrderStarted =>
              orderIdToStarted(orderId) = timestamp
              IO.unit

            case _: OrderTerminated =>
              for start <- orderIdToStarted.remove(orderId) do
                val duration = timestamp - start
                totalOrderDuration += duration
                maximumOrderDuration = maximumOrderDuration max duration
              IO.unit

            case _: OrderProcessingStarted =>
              orderIdToProcessingStarted(orderId) = timestamp
              IO.unit

            case _: OrderProcessed =>
              for start <- orderIdToProcessingStarted.remove(orderId) do
                processedCount += 1
                val duration = timestamp - start
                totalProcessDuration += duration
                maximumProcessDuration = maximumProcessDuration max duration
              IO.unit

            case OrderForked(children) =>
              orderToChildren(orderId) = children.map(_.orderId)
              IO.unit

            case _: OrderJoined =>
              for children <- orderToChildren.remove(orderId) do
                completedForkedOrderCount += children.size
              obs.tryPublish()

            case _: OrderCancelled | _: OrderFailed | _: OrderFailedInFork =>
              failedOrderCount += 1
              IO.unit

            case _ =>
              IO.unit
        case _ =>
          IO.unit

  def toStatistics = Statistics(
    since.elapsed,
    lastOrderCount = lastOrderCount,
    eventCount = eventCount,
    completedOrderCount = _deletedOrderCount,
    failedOrderCount = failedOrderCount,
    totalOrderDuration = totalOrderDuration,
    maximumOrderDuration = maximumOrderDuration,
    completedForkedOrderCount = completedForkedOrderCount,
    processedCount = processedCount,
    totalProcessDuration = totalProcessDuration,
    maximumProcessDuration = maximumProcessDuration,
    stdWritten = stdWritten)
