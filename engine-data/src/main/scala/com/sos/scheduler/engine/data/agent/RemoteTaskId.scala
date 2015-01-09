package com.sos.scheduler.engine.data.agent

import com.sos.scheduler.engine.data.agent.RemoteTaskId._
import java.util.concurrent.ThreadLocalRandom
import scala.math.abs

/**
 * @author Joacim Zschimmer
 */
final case class RemoteTaskId(value: Long) {
  def string = value.toString
  def index = value / Factor
}

object RemoteTaskId {
  private val Factor = 1000*1000*1000L
  private val MaxIndex = Int.MaxValue

  /**
   * Delivers RemoteTaskId with recognizable increasing numbers.
   * The increasing number is meaningless.
   */
  def newGenerator(start: Int = 1): Iterator[RemoteTaskId] = {
    val numbers = Iterator.range(start, MaxIndex) ++ Iterator.continually { Iterator.range(1, MaxIndex) }.flatten
    numbers map { i ⇒ RemoteTaskId.apply(salt(i)) }
  }

  private def salt(i: Long) = i * Factor + abs(ThreadLocalRandom.current.nextLong()) % Factor
}
