package com.sos.jobscheduler.data

import com.sos.jobscheduler.data.order.{LeanOrder, Payload}

/**
  * @author Joacim Zschimmer
  */
package object workflow {
  type NodeToLeanOrder = PartialFunction[NodeId, LeanOrder]
  type NodeToPayload = PartialFunction[NodeId, Payload]
}
