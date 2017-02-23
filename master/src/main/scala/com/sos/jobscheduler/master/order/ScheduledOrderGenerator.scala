package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.data.engine2.order.NodeKey
import com.sos.jobscheduler.master.oldruntime.OldSchedule

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  path: OrderGeneratorPath,
  nodeKey: NodeKey,
  variables: Map[String, String],
  schedule: OldSchedule)
