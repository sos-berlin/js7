package com.sos.scheduler.engine.agent.data.commandresponses

/**
 * @author Joacim Zschimmer
 */
trait XmlResponse extends Response {
  def toXmlElem: xml.Elem
}
