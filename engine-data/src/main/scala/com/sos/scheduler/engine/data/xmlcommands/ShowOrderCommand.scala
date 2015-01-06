package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.order.OrderKey

final case class ShowOrderCommand(
    orderKey: OrderKey,
    historyId: Option[Int] = None,
    what: Iterable[What] = Nil)
extends XmlCommand {

  def xmlElem =
      <show_order
        job_chain={orderKey.jobChainPath.string}
        order={orderKey.id.string}
        history_id={(historyId map { _.toString }).orNull}
        what={what mkString " "}/>
}
