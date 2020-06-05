package js7.core.command

import js7.base.auth.UserId
import js7.base.time.ScalaTime._
import js7.data.command.{CommandRunOverview, CommonCommand, InternalCommandId}
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
  override def toString = s"Command $idString by User '${userId.string}': ${command.toShortString}"

  def idString = batchInternalId match {
    case None => internalId.toString  // #101
    case Some(batchId) => s"$batchId+${internalId.number - batchId.number}"  // #100+1
  }

  def overview = new CommandRunOverview(internalId, runningSince.elapsed, command)
}
