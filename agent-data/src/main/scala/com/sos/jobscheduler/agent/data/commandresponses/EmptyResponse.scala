package com.sos.jobscheduler.agent.data.commandresponses

import spray.json.DefaultJsonProtocol.jsonFormat0

/**
 * @author Joacim Zschimmer
 */
case object EmptyResponse extends XmlResponse {

  def toXmlElem = <ok/>

  implicit val MyJsonFormat = jsonFormat0(() ⇒ EmptyResponse)
}
