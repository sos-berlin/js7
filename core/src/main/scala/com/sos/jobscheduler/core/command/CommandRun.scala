package com.sos.jobscheduler.core.command

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.data.command.{CommandRunOverview, CommonCommand, InternalCommandId}
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final case class CommandRun[C <: CommonCommand](
  internalId: InternalCommandId,
  userId: UserId,
  command: C,
  runningSince: Deadline,
  batchInternalId: Option[InternalCommandId])
{
  override def toString = s"$idString (User ${userId.string}) ${command.toShortString}"

  def idString = batchInternalId match {
    case None => internalId.toString  // #101
    case Some(batchId) => s"$batchId+${internalId.number - batchId.number}"  // #100+1
  }

  def overview = new CommandRunOverview(internalId, runningSince.elapsed, command)
}
