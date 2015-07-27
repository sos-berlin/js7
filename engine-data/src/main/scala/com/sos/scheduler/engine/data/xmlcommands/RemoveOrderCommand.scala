package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.order.OrderKey

/**
 * @author Joacim Zschimmer
 */
final case class RemoveOrderCommand(orderKey: OrderKey)
extends XmlCommand {

  def xmlElem: xml.Elem = <remove_order job_chain={orderKey.jobChainPath.string} order={orderKey.id.string}/>
}
