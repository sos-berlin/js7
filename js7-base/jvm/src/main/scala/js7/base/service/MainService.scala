package js7.base.service

import cats.effect.IO
import js7.base.utils.ProgramTermination

trait MainService extends Service:

  protected type Termination <: ProgramTermination

  def untilTerminated: IO[Termination]
