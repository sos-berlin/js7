package js7.subagent

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.base.utils.{Allocated, ProgramTermination}
import js7.subagent.ConvertibleToDirector.*
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.Scheduler

final class ConvertibleToDirector {
  private val restartAsDirectorVar = Deferred.unsafe[Task, Unit]

  def resource(conf: SubagentConf, scheduler: Scheduler): Resource[Task, Subagent] =
    BareSubagent.resource(conf, scheduler,
      convertToDirector = restartAsDirectorVar.complete(()).attempt.void)

  def use(subagentAllocated: Allocated[Task, Subagent])
  : Task[Either[ConvertToDirector, ProgramTermination]] =
    Task
      .race(restartAsDirectorVar.get, subagentAllocated.allocatedThing.untilTerminated)
      .flatMap {
        case Left(()) =>
          subagentAllocated.allocatedThing.shutdown(dontWaitForDirector = true)
            .*>(subagentAllocated.stop)
            .as(Left(ConvertToDirector))

        case Right(o) =>
          Task.right(o)
      }
}

object ConvertibleToDirector {
  def convertibleToDirector[A](body: ConvertibleToDirector => A): A =
    body(new ConvertibleToDirector)

  type ConvertToDirector = ConvertToDirector.type

  object ConvertToDirector
}