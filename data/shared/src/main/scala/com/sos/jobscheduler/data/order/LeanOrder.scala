package com.sos.jobscheduler.data.order

/**
  * @author Joacim Zschimmer
  */
final case class LeanOrder(id: OrderId, payload: Payload) {

  def outcome = payload.outcome
}
