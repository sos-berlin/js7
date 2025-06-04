package js7.core.command

import cats.effect.{Resource, Sync}
import js7.base.log.CorrelId
import js7.data.command.CommonCommand
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class CommandRegister[C <: CommonCommand]:

  private var totalCounter = 0L
  private val idToCommand = mutable.Map.empty[CorrelId, CommandRun[C]]

  def resource[F[_]: Sync as F](
    command: C, meta: CommandMeta, correlId: CorrelId, batchId: Option[CorrelId])
  : Resource[F, CommandRun[C]] =
    Resource.make(
      acquire = F.delay(add(command, meta, correlId, batchId)))(
      release = run => F.delay(remove(run.correlId)))

  def add(command: C, meta: CommandMeta, correlId: CorrelId, batchId: Option[CorrelId])
  : CommandRun[C] =
    synchronized:
      totalCounter += 1
      val run = CommandRun[C](correlId, meta.user.id, command, now, batchId)
      idToCommand.update(correlId, run)
      run

  def remove(id: CorrelId): Unit =
    synchronized:
      idToCommand -= id
