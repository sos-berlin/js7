package com.sos.scheduler.engine.agent.task

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.StartTask
import java.net.InetAddress

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[StandardAgentTaskFactory])
trait AgentTaskFactory extends ((StartTask, Option[InetAddress]) ⇒ AgentTask) {

  def apply(command: StartTask, clientIpOption: Option[InetAddress]): AgentTask
}
