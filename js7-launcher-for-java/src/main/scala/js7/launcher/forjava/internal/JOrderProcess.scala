package js7.launcher.forjava.internal

import cats.effect.IO
import java.util.concurrent.CompletionStage
import js7.base.catsutils.CatsExtensions.tryIt
import js7.data.order.OrderOutcome
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOutcome
import js7.launcher.OrderProcess
import scala.jdk.FutureConverters.*

final case class JOrderProcess(asScala: OrderProcess)
extends JavaWrapper:

  type AsScala = OrderProcess


object JOrderProcess:

  def of(outcome: CompletionStage[JOutcome.Completed]): JOrderProcess =
    JOrderProcess(OrderProcess(
      IO.fromFuture(IO.pure(outcome.asScala))
        .map(_.asScala)
        .tryIt
        .map(OrderOutcome.Completed.fromTry)))
