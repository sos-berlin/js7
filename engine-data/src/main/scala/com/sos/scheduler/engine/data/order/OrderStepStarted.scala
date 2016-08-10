package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.ModifiableSourceEvent
import com.sos.scheduler.engine.data.job.TaskId

final case class OrderStepStarted(orderKey: OrderKey, state: OrderState, taskId: TaskId)
extends OrderEvent with ModifiableSourceEvent
