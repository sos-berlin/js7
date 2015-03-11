package com.sos.scheduler.engine.taskserver

/**
 * @author Joacim Zschimmer
 */
trait TaskServer extends AutoCloseable {
  def start(): Unit
  def kill(): Unit
}
