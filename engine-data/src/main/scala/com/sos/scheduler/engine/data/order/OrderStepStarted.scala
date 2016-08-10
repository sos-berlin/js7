package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.ModifiableSourceEvent
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobchain.NodeId

final case class OrderStepStarted(orderKey: OrderKey, nodeId: NodeId, taskId: TaskId)
extends OrderEvent with ModifiableSourceEvent
