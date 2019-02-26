package com.sos.jobscheduler.core.command

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.command.{CommandRunOverview, CommonCommand, InternalCommandId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class CommandRun[C <: CommonCommand](
  internalId: InternalCommandId,
  command: C,
  startedAt: Timestamp,
  batchInternalId: Option[InternalCommandId])
{
  override def toString = s"$idString ${command.toShortString}"

  def idString = batchInternalId match {
    case None => internalId.toString  // #101
    case Some(batchId) => s"$batchId+${internalId.number - batchId.number}"  // #100+1
  }

  def overview = new CommandRunOverview(internalId, startedAt, command)
}
