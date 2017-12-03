package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.LeanOrder

case object ForwardTransition extends OneToOneTransition {

  def result(order: LeanOrder) = 0
}
