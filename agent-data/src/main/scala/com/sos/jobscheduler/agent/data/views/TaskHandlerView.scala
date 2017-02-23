package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
trait TaskHandlerView {

  def overview: TaskHandlerOverview

  def taskOverviews: immutable.Seq[TaskOverview]

  def taskOverview(id: AgentTaskId): TaskOverview
}
