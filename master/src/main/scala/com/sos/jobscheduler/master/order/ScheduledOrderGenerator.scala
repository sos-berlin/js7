package com.sos.scheduler.engine.master.order

import com.sos.scheduler.engine.data.engine2.order.NodeKey
import com.sos.scheduler.engine.master.oldruntime.OldSchedule

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  path: OrderGeneratorPath,
  nodeKey: NodeKey,
  variables: Map[String, String],
  schedule: OldSchedule)
