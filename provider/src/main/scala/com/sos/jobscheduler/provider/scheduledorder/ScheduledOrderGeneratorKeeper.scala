package com.sos.jobscheduler.provider.scheduledorder

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.provider.scheduledorder.ScheduledOrderGeneratorKeeper._
import com.sos.jobscheduler.provider.scheduledorder.oldruntime.InstantInterval

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorKeeper(scheduledOrderGenerators: Iterable[ScheduledOrderGenerator]) {

  private val pathToOrderGenerator: Map[ScheduledOrderGeneratorPath, ScheduledOrderGenerator] =
    scheduledOrderGenerators toKeyedMap (_.path)

  def generateOrders(instantInterval: InstantInterval): Seq[FreshOrder] =
    (for {
      orderGenerator <- pathToOrderGenerator.values
      instant <- orderGenerator.schedule.instants(instantInterval)
    } yield
      FreshOrder(
        toOrderId(orderGenerator.path, instant.toTimestamp),
        orderGenerator.workflowPath,
        Some(instant.toTimestamp),
        orderGenerator.arguments))
    .toVector.sortBy { _.scheduledFor }
}

object ScheduledOrderGeneratorKeeper {
  private val Separator = "@"

  private def toOrderId(path: ScheduledOrderGeneratorPath, timestamp: Timestamp) =
    OrderId(s"${path.withoutStartingSlash.replace('/', '-')}$Separator$timestamp")  // '/' is a reserved character in OrderId
}
