package js7.core.command

import js7.base.auth.UserId
import js7.base.log.CorrelId
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
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
  // CorrelId is not relevant for the User, and debug log should show the CorrelId.
  override def toString =
    s"Command by $userId: ${command.toShortString}"

  def idString = "#" + batchInternalId.match {
    case None => correlId.toString
    case Some(batchId) => s"$batchId.$correlId"
  }

  def overview = new CommandRunOverview(correlId, runningSince.elapsed, command)
}
