package js7.core.command

import js7.base.auth.UserId
import js7.base.log.CorrelId
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.{CommandRunOverview, CommonCommand}
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final case class CommandRun[C <: CommonCommand](
  correlId: CorrelId,
  userId: UserId,
  command: C,
  runningSince: Deadline,
  batchInternalId: Option[CorrelId])
{
  override def toString = s"Command $idString by User '${userId.string}': ${command.toShortString}"

  def idString = "#" + batchInternalId.match_ {
    case None => correlId.toString
    case Some(batchId) => s"$batchId.$correlId"
  }

  def overview = new CommandRunOverview(correlId, runningSince.elapsed, command)
}
