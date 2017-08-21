package com.sos.jobscheduler.agent.data.commandresponses

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import spray.json.DefaultJsonProtocol.jsonFormat0

/**
 * @author Joacim Zschimmer
 */
case object EmptyResponse extends AgentCommand.Response {

  def toXmlElem = <ok/>

  implicit val MyJsonFormat = jsonFormat0(() ⇒ EmptyResponse)
}
