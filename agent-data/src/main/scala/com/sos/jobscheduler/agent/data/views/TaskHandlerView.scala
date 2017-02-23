package com.sos.scheduler.engine.agent.data.views

import com.sos.scheduler.engine.agent.data.AgentTaskId
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
trait TaskHandlerView {

  def overview: TaskHandlerOverview

  def taskOverviews: immutable.Seq[TaskOverview]

  def taskOverview(id: AgentTaskId): TaskOverview
}
