package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.order.{OrderKey, OrderState}
import com.sos.scheduler.engine.data.time.SchedulerDateTime
import java.time.Instant

final case class OrderCommand(
    orderKey: OrderKey,
    state: Option[OrderState] = None,
    at: Option[Instant] = None,
    suspended: Option[Boolean] = None,
    title: Option[String] = None,
    parameters: Map[String, String] = Map(),
    xmlChildren: xml.NodeSeq = Nil)
extends XmlCommand {

  def xmlElem: xml.Elem = {
    val parameterElem: xml.NodeSeq =
      if (parameters.isEmpty) Nil
      else <params>{parameters map { case (k, v) ⇒ <param name={k} value={v}/> }}</params>;
    <order
        job_chain={orderKey.jobChainPath.string}
        id={orderKey.id.string}
        state={(state map { _.string }).orNull}
        at={(at map SchedulerDateTime.formatUtc).orNull}
        suspended={(suspended map { _.toString }).orNull}
        title={title.orNull}>{
      parameterElem}{
      xmlChildren
    }</order>
  }
}
