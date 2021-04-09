package js7.executor

import js7.base.io.process.ProcessSignal
import js7.data.order.Outcome
import monix.eval.Task

final case class OrderProcess private(
  run: Task[Outcome.Completed],
  cancel: ProcessSignal => Unit = _ => ())
