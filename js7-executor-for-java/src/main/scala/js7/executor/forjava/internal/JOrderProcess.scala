package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.data_for_java.common.JavaWrapper
import js7.executor.internal.InternalJob.OrderProcess
import monix.eval.Task
import scala.jdk.FutureConverters._

final case class JOrderProcess(asScala: OrderProcess)
extends JavaWrapper
{
  type AsScala = OrderProcess
}

object JOrderProcess
{
  def of(completed: java.util.concurrent.CompletionStage[JOrderResult]): JOrderProcess =
    JOrderProcess(OrderProcess(
      Task.fromFuture(completed.asScala)
        .map(_.asScala)
        .materialize.map(Checked.fromTry)
        .memoize))
}
