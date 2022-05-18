package js7.core.command

import js7.base.log.CorrelId
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview, CommonCommand}
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class CommandRegister[C <: CommonCommand]
{
  private var totalCounter = 0L
  private val idToCommand = mutable.Map.empty[CorrelId, CommandRun[C]]

  def add(command: C, meta: CommandMeta, correlId: CorrelId, batchId: Option[CorrelId])
  : CommandRun[C] =
    synchronized {
      totalCounter += 1
      val run = CommandRun[C](correlId, meta.user.id, command, now, batchId)
      idToCommand.update(correlId, run)
      run
    }

  def remove(id: CorrelId): Unit =
    synchronized {
      idToCommand -= id
    }

  def overview: CommandHandlerOverview =
    synchronized {
      CommandHandlerOverview(currentCommandCount = idToCommand.size, totalCommandCount = totalCounter)
    }

  def detailed: CommandHandlerDetailed[C] =
    synchronized {
      CommandHandlerDetailed(idToCommand.values.map(_.overview).toVector)
    }
}
