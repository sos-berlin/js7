package com.sos.scheduler.engine.agent.task

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.StartTask

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[StandardAgentTaskFactory])
trait AgentTaskFactory extends (StartTask ⇒ AgentTask)
