package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.AbstractEvent
import com.sos.scheduler.engine.data.jobchain.NodeId

final case class OrderNodeChanged(orderKey: OrderKey, fromNodeId: NodeId, nodeId: NodeId)
extends AbstractEvent  // Für @ForCpp: Funktionen müssen eine Klasse, kein Interface liefern
with OrderEvent {

  def nodeIdChange: (NodeId, NodeId) = fromNodeId → nodeId
}
