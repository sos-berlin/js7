package js7.base.service

import cats.effect.kernel.Outcome
import cats.effect.{Deferred, IO, ResourceIO}
import js7.base.utils.ProgramTermination

trait SimpleMainService extends MainService:

  protected type Termination = ProgramTermination

  private val terminated = Deferred.unsafe[IO, Termination]

  final def start: IO[Service.Started] =
    startService:
      run.guaranteeCase:
        case Outcome.Succeeded(o) => o.flatMap(terminated.complete).void
        case Outcome.Errored(t) => terminated.complete(ProgramTermination.Failure).void
        case Outcome.Canceled() => terminated.complete(ProgramTermination.Failure).void
      .void

  override final def untilTerminated: IO[ProgramTermination] =
    terminated.get

  def run: IO[Termination]


object SimpleMainService:

  def resource(program: IO[ProgramTermination], label: String = ""): ResourceIO[SimpleMainService] =
    Service.resource:
      IO:
        new SimpleMainService with Service.StoppableByRequest:
          def run = program

          override def toString =
            if label.nonEmpty then label else super.toString
