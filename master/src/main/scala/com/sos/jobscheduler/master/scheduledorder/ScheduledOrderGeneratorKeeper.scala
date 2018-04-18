package com.sos.jobscheduler.master.scheduledorder

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId, Payload}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.oldruntime.InstantInterval
import com.sos.jobscheduler.master.scheduledorder.ScheduledOrderGeneratorKeeper._
import java.time.Instant
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorKeeper(masterConfiguration: MasterConfiguration, scheduledOrderGenerators: Iterable[ScheduledOrderGenerator]) {

  private val pathToOrderGenerator: Map[ScheduledOrderGeneratorPath, ScheduledOrderGenerator] =
    scheduledOrderGenerators toKeyedMap (_.path)

  def generateOrders(instantInterval: InstantInterval): Seq[FreshOrder] =
    (for (orderGenerator ← pathToOrderGenerator.values;
          instant ← orderGenerator.schedule.instants(instantInterval)) yield
      FreshOrder(
        toOrderId(orderGenerator.path, instant),
        orderGenerator.workflowPath,
        Some(instant.toTimestamp),
        Payload(orderGenerator.variables)))
    .toVector.sortBy { _.scheduledAt }
}

object ScheduledOrderGeneratorKeeper {
  private val Separator = "@"

  private def toOrderId(path: ScheduledOrderGeneratorPath, instant: Instant) =
    OrderId(s"${path.string}$Separator$instant")
}
