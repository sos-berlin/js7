package com.sos.scheduler.engine.taskserver.task

/**
 * @author Joacim Zschimmer
 */
trait Task extends AutoCloseable {

  def start(): Boolean

  def end(): Unit

  def step(): Any

  def callIfExists(methodWithSignature: String): Any
}
