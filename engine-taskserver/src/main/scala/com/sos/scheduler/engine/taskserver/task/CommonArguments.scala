package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[task] final case class CommonArguments(
  agentTaskId: AgentTaskId,
  jobName: String,
  namedInvocables: NamedInvocables,
  monitors: immutable.Seq[Monitor],
  hasOrder: Boolean,
  stdFiles: StdFiles)
