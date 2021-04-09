package js7.executor.forjava.internal

import java.util.concurrent.CompletionStage
import js7.data.order.Outcome
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOutcome
import js7.executor.OrderProcess
import monix.eval.Task
import scala.jdk.FutureConverters._

final case class JOrderProcess(asScala: OrderProcess)
extends JavaWrapper
{
  type AsScala = OrderProcess
}

object JOrderProcess
{
  def of(completed: CompletionStage[JOutcome.Completed]): JOrderProcess =
    JOrderProcess(OrderProcess(
      Task.fromFuture(completed.asScala)
        .map(_.asScala)
        .materialize
        .map(Outcome.Completed.fromTry)))
}
