package js7.base.utils

import monix.eval.Task

trait Stoppable
{
  def stop: Task[Unit]
}
