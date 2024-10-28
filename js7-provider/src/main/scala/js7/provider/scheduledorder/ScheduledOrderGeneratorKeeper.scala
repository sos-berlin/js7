package js7.provider.scheduledorder

import js7.base.time.JavaTimeConverters.*
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.order.{FreshOrder, OrderId}
import js7.provider.scheduledorder.ScheduledOrderGeneratorKeeper.*
import js7.provider.scheduledorder.oldruntime.InstantInterval

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorKeeper(scheduledOrderGenerators: Iterable[ScheduledOrderGenerator]):

  private val pathToOrderGenerator: Map[ScheduledOrderGeneratorPath, ScheduledOrderGenerator] =
    scheduledOrderGenerators.toKeyedMap(_.path)

  def generateOrders(instantInterval: InstantInterval): Seq[FreshOrder] =
    (for
      orderGenerator <- pathToOrderGenerator.values
      instant <- orderGenerator.schedule.instants(instantInterval)
    yield
      FreshOrder(
        toOrderId(orderGenerator.path, instant.toTimestamp),
        orderGenerator.workflowPath,
        orderGenerator.arguments,
        Some(instant.toTimestamp)))
    .toVector.sortBy(_.scheduledFor)


object ScheduledOrderGeneratorKeeper:
  private val Separator = "@"

  private def toOrderId(path: ScheduledOrderGeneratorPath, timestamp: Timestamp) =
    OrderId(s"${path.string.replace('/', '-')}$Separator$timestamp")  // '/' is a reserved character in OrderId
