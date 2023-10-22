package js7.base.service

import cats.effect.Resource
import cats.effect.IO

final class CancelableService private(protected val run: IO[Unit])
extends Service.StoppableByRequest:

  protected def start =
    startService(IO
      .race(untilStopRequested, run) // Cancels run
      .map(_.fold(identity, identity)))

  override def toString = "CancelableService"


object CancelableService:
  def resource(run: IO[Unit]): Resource[IO, CancelableService] =
    Service.resource(IO(new CancelableService(run)))
