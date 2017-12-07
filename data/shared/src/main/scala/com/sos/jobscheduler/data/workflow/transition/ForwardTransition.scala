package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.workflow.transition.TransitionType.{Move, Outlet}
import scala.collection.immutable.IndexedSeq

case object ForwardTransition extends SingleInputTransition {

  val outletsMaximum = Some(1)

  def result(order: InputOrder, outlets: IndexedSeq[Outlet]) =
    Move(outlets(0))
}
