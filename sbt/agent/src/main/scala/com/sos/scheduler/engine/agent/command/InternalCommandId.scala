package com.sos.scheduler.engine.agent.command

import com.sos.scheduler.engine.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class InternalCommandId(value: Long) extends IsString {
  def string = value.toString
  override def toString = s"#$value"
}

object InternalCommandId extends IsString.HasJsonFormat[InternalCommandId] {
  def apply(o: String) = InternalCommandId(o.toLong)
}
