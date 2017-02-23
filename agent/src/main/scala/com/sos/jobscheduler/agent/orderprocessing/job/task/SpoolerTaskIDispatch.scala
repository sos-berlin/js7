package com.sos.scheduler.engine.agent.orderprocessing.job.task

import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, InvocableIDispatch}
import com.sos.scheduler.engine.minicom.remoting.proxy.{HasProxyMeta, ProxyMeta}
import com.sos.scheduler.engine.taskserver.spoolerapi.ProxySpoolerTask
import scala.annotation.meta.getter
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class SpoolerTaskIDispatch(initialTaskVariables: Map[String, String])
extends InvocableIDispatch with AnnotatedInvocable with HasProxyMeta {

  val proxyMeta = ProxyMeta(ProxySpoolerTask.clsid, Map())

  private var _taskVariables = mutable.Map[String, String]() ++ initialTaskVariables
  @(getter @invocable(dispId = 13) @getter)
  var order: OrderIDispatch = _

  @invocable(dispId = 35)
  def params_xml: String =
    VariableSets.toXmlElem(_taskVariables).toString

  @invocable(dispId = 35)
  def params_xml_=(o: String): Unit = {
    _taskVariables = mutable.Map[String, String]() ++ VariableSets.parseXml(o)
  }

  @invocable(dispId = 36)
  def order_params_xml: String =
    if (order == null)
      VariableSets.toXmlElem(Map()).toString
    else
      VariableSets.toXmlElem(order.variables).toString

  @invocable(dispId = 36)
  def order_params_xml_=(o: String): Unit = {
    order.mergeVariables(VariableSets.parseXml(o))
  }
}
