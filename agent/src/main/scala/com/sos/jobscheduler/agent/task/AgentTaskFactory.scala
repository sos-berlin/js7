package com.sos.jobscheduler.agent.task

import com.google.inject.ImplementedBy
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import java.net.InetAddress

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[StandardAgentTaskFactory])
trait AgentTaskFactory {

  def apply(command: AgentCommand.StartTask, clientIpOption: Option[InetAddress]): AgentTask
}
