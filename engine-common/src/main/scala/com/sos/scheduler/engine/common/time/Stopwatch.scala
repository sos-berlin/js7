package com.sos.scheduler.engine.common.time

import java.lang.System.currentTimeMillis
import java.time.Duration

/**
 * @author Joacim Zschimmer
 */
final class Stopwatch {
  private val start = currentTimeMillis()

  def elapsedMs = currentTimeMillis - start

  def duration = Duration.ofMillis(elapsedMs)

  override def toString = s"${elapsedMs / 1000.0}s"
}
