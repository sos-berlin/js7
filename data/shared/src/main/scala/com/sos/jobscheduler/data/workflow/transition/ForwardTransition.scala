package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Move
import scala.collection.immutable.Seq

case object ForwardTransition extends SingleInputTransition {

  val graphsMaximum = Some(1)

  def result(order: InputOrder, childIds: Seq[OrderId.Child]) =
    Move(0)
}
