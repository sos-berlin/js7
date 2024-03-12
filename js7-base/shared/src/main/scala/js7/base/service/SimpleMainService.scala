package js7.base.service

import cats.effect.{Deferred, IO}
import js7.base.utils.ProgramTermination

trait SimpleMainService extends MainService:

  protected type Termination = ProgramTermination

  private val terminated = Deferred.unsafe[IO, Termination]

  final def start =
    startService:
      for
        termination <- run
        _ <- terminated.complete(termination)
      yield
        ()

  override final def untilTerminated =
    terminated.get

  def run: IO[Termination]
