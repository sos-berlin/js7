package js7.base.service

import cats.effect.{IO, ResourceIO}

final class CancelableService private(protected val run: IO[Unit])
extends Service.StoppableByRequest:

  protected def start =
    startService(IO
      .race(untilStopRequested, run) // Cancels run
      .map(_.merge))

  override def toString = "CancelableService"


object CancelableService:
  def resource(run: IO[Unit]): ResourceIO[CancelableService] =
    Service.resource(IO(new CancelableService(run)))
