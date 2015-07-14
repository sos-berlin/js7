package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

final case class SimpleJobNodeOverview(
  orderState: OrderState,
  nextState: OrderState,
  errorState: OrderState,
  action: JobChainNodeAction,
  jobPath: JobPath,
  orderCount: Int)
extends JobNodeOverview


object SimpleJobNodeOverview {
  private implicit val OrderStateJsonFormat = OrderState.MyJsonFormat
  private implicit val JobChainNodeActionJsonFormat = JobChainNodeAction.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat6(apply)
}
