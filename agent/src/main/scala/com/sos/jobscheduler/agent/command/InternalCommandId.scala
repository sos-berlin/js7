package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.base.generic.IsString

/**
 * @author Joacim Zschimmer
 */
final case class InternalCommandId(number: Long) extends IsString {
  def string = number.toString
  override def toString = s"#$number"
}

object InternalCommandId extends IsString.HasJsonCodec[InternalCommandId] {
  def apply(o: String) = InternalCommandId(o.toLong)

  def newGenerator(): Iterator[InternalCommandId] =
    Iterator.iterate(1L) { _ + 1 } map InternalCommandId.apply
}
