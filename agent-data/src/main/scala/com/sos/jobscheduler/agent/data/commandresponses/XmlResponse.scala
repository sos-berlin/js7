package com.sos.jobscheduler.agent.data.commandresponses

import com.sos.jobscheduler.agent.data.commands.AgentCommand

/**
 * @author Joacim Zschimmer
 */
trait XmlResponse extends AgentCommand.Response {
  def toXmlElem: xml.Elem
}
