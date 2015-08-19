package com.sos.scheduler.engine.agent.command

import com.google.inject.ImplementedBy
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandHandlerDetails  {
  def commandRuns: immutable.Iterable[CommandRunOverview]
}
