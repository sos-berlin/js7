package com.sos.jobscheduler.agent.task

import com.google.inject.ImplementedBy
import java.net.InetAddress

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[StandardAgentTaskFactory])
trait AgentTaskFactory {

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask
}
