package com.sos.scheduler.engine.common.time.timer.signaling

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit._

/**
  * Slow. Use [[SynchronizedSignaling]].
  * @author Joacim Zschimmer
  */
private[timer] final class BlockingQueueSignaling extends Signaling {

  private val blockingQueue = new ArrayBlockingQueue[java.lang.Boolean](1)

  def signal() = blockingQueue.offer(true)

  def awaitMillis(millis: Long) = blockingQueue.poll(millis, MILLISECONDS) != null

  def await(): Unit = blockingQueue.take()

  def isSignaled = !blockingQueue.isEmpty
}
