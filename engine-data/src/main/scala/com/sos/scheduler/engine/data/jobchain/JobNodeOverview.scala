package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

final case class JobNodeOverview(
  orderState: OrderState,
  nextState: OrderState,
  errorState: OrderState,
  action: JobChainNodeAction,
  jobPath: JobPath,
  orderCount: Int)
extends OrderQueueNodeOverview


object JobNodeOverview {
  private implicit val OrderStateJsonFormat = OrderState.MyJsonFormat
  private implicit val JobChainNodeActionJsonFormat = JobChainNodeAction.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat6(apply)
}
