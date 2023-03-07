package js7.base.service

import js7.base.utils.ProgramTermination
import monix.eval.Task

trait MainService extends Service {
  def untilTerminated: Task[ProgramTermination]
}
