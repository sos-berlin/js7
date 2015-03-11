package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.data.agent.RemoteTaskId

/**
 * @author Joacim Zschimmer
 */
trait RemoteTask extends AutoCloseable {

  def id: RemoteTaskId

  def start(): Unit

  def kill(): Unit
}
