package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.taskserver.spoolerapi.SpoolerTask._
import com.sos.scheduler.engine.taskserver.task.common.VariableSets

/**
 * @author Joacim Zschimmer
 */
trait SpoolerTask extends Invocable {

  def setErrorCodeAndText(code: String, text: String): Unit

  final def parameterMap: Map[String, String] = xmlToParameterMap(paramsXml)

  def paramsXml: String

  def paramsXml_=(o: String): Unit

  final def orderParameterMap: Map[String, String] = xmlToParameterMap(orderParamsXml)

  def orderParamsXml: String

  def orderParamsXml_=(o: String): Unit
}

private object SpoolerTask {
  final def xmlToParameterMap(xmlString: String): Map[String, String] =
    xmlString match {
      case "" ⇒ Map()
      case o ⇒ VariableSets.parseXml(o)
    }
}
