package com.sos.scheduler.engine.data.agent

import com.sos.scheduler.engine.data.base.GenericLong
import java.util.concurrent.ThreadLocalRandom
import scala.math.abs

/**
 * @author Joacim Zschimmer
 */
final case class AgentProcessId(value: Long) extends GenericLong {
  import com.sos.scheduler.engine.data.agent.AgentProcessId._
  def string = value.toString
  def index = value / Factor
}

object AgentProcessId extends GenericLong.HasJsonFormat[AgentProcessId] {

  private val Factor = 1000*1000*1000L
  private val MaxIndex = Int.MaxValue

  /**
   * Delivers [[AgentProcessId]] with recognizable increasing numbers.
   * The increasing number is meaningless.
   */
  def newGenerator(start: Int = 1): Iterator[AgentProcessId] = {
    val numbers = Iterator.range(start, MaxIndex) ++ Iterator.continually { Iterator.range(1, MaxIndex) }.flatten
    numbers map { i ⇒ AgentProcessId.apply(salt(i)) }
  }

  private def salt(i: Long) = i * Factor + abs(ThreadLocalRandom.current.nextLong()) % Factor
}
