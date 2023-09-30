package js7.launcher.forjava.internal

import java.util.concurrent.CompletionStage
import js7.data.order.Outcome
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOutcome
import js7.launcher.OrderProcess
import monix.eval.Task
import scala.jdk.FutureConverters.*

final case class JOrderProcess(asScala: OrderProcess)
extends JavaWrapper:

  type AsScala = OrderProcess

object JOrderProcess:
  def of(outcome: CompletionStage[JOutcome.Completed]): JOrderProcess =
    JOrderProcess(OrderProcess(
      Task.fromFuture(outcome.asScala)
        .map(_.asScala)
        .materialize
        .map(Outcome.Completed.fromTry)))
