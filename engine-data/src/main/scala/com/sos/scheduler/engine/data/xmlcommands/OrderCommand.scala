package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.order.{OrderKey, OrderState}

final case class OrderCommand(
    orderKey: OrderKey,
    state: Option[OrderState] = None,
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
        state={(state map {_.string}).orNull}
        title={title.orNull}>{parameterElem}{xmlChildren}</order>
  }
}
