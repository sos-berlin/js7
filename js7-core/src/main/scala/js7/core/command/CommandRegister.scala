package js7.core.command

import cats.effect.{Resource, Sync}
import js7.base.log.CorrelId
import js7.data.command.CommonCommand
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class CommandRegister[C <: CommonCommand]:

  //could be used in am informative web service or something like that:
  //private val idToCommand = TrieMap.empty[CorrelId, CommandRun[C]]

  def resource[F[_]: Sync as F](
    command: C, meta: CommandMeta, correlId: CorrelId, batchId: Option[CorrelId])
  : Resource[F, CommandRun[C]] =
    Resource.make(
      acquire = F.delay(add(command, meta, correlId, batchId)))(
      release = run => F.delay(remove(run.correlId)))

  private def add(command: C, meta: CommandMeta, correlId: CorrelId, batchId: Option[CorrelId])
  : CommandRun[C] =
    val run = CommandRun[C](correlId, meta.user.id, command, now, batchId)
    //idToCommand.update(correlId, run)
    run

  private def remove(id: CorrelId): Unit =
    () //idToCommand -= id
